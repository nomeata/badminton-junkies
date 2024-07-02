{
    inputs = {
        # Here you can adjust the IHP version of your project
        # You can find new releases at https://github.com/digitallyinduced/ihp/releases
        ihp.url = "github:digitallyinduced/ihp/v1.3";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, nixpkgs, self, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                        hlint
                        # custom deps
                        tz
                        wreq
                        tagsoup
                        modern-uri
                        ((pkgs.haskell.lib.markUnbroken iCalendar).overrideAttrs {
                          src = pkgs.fetchFromGitHub {
                            owner = "nomeata";
                            repo = "iCalendar";
                            rev = "82f9d0b33e4f9e81f04f25ff34bd872c56d7045c";
                            hash = "sha256-HZfYCVkjp0F9cbhQHIW9s4RWiwn5K1Gqe0X/di7yL3A=";
                          };
                        })
                    ];
                };
            };

           flake.nixosConfigurations."badjunk" = nixpkgs.lib.nixosSystem {
             system = "x86_64-linux";
             specialArgs = inputs;
             modules = [
               "${nixpkgs}/nixos/modules/profiles/qemu-guest.nix"
               ihp.nixosModules.appWithPostgres
               ({ pkgs, ... }: {

                   boot.loader.grub.device = "/dev/sda";
                   boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi" ];
                   boot.initrd.kernelModules = [ "nvme" ];
                   fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

                   nix.settings.substituters = [
                     "https://digitallyinduced.cachix.org"
                     "https://cache.garnix.io"
                   ];
                   nix.settings.trusted-public-keys = [
                     "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
                     "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
                   ];

                   services.openssh.enable = true;
                   services.openssh.settings.PermitRootLogin = "prohibit-password";
                   users.users.root.openssh.authorizedKeys.keys = [''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRd0CZZQXyKTEQSEtrIpcTg15XEoRjuYodwo0nr5hNj jojo@kirk'' ];

                   security.acme.defaults.email = "mail@joachim-breitner.de";

                   networking.firewall.allowedTCPPorts = [ 80 22 443 ];

                   systemd.services.worker.enable = pkgs.lib.mkForce false;
                   services.ihp = {
                       httpsEnabled = true;
                       domain = "badjunk.nomeata.de";
                       migrations = ./Application/Migration;
                       schema = ./Application/Schema.sql;
                       fixtures = ./Application/Fixtures.sql;
                       sessionSecret = "xxx";
                       minimumRevision = 1674294934;
                   };

                   # Add swap to avoid running out of memory during builds
                   # Useful if your server have less than 4GB memory
                   #swapDevices = [ { device = "/swapfile"; size = 8192; } ];

                   # This should reflect the nixos version from the NixOS AMI initally installed
                   # After the initial install, it should not be changed. Otherwise e.g. the postgres
                   # server might need a manual data migration if NixOS changes the default postgres version
                   system.stateVersion = "23.05";
               })
              ];
           };
        };
}
