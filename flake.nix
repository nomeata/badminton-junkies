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
             specialArgs = inputs // {
               environment = "production";
               ihp-migrate = self.packages.x86_64-linux.migrate;
               ihpApp = self.packages.x86_64-linux.default;
             };
             modules = [
               ./nixos/configuration.nix
             ];
           };
        };
}
