{
    inputs = {
        # Here you can adjust the IHP version of your project
        # You can find new releases at https://github.com/digitallyinduced/ihp/releases
        # 1.1+a few
        ihp.url = "github:digitallyinduced/ihp/217c1aaef7877220cc510714a81e25be2f019d22";
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
