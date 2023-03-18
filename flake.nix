{
  description = "Shipnix server configuration for badjunk";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    ihp.url = github:digitallyinduced/ihp/v1.0.1;
    ihp.flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ihp } @attrs:
    let
      system = "x86_64-linux";
      overlay-unstable = final: prev: {
        unstable = nixpkgs-unstable.legacyPackages.${prev.system};
        # use this variant if unfree packages are needed:
        # unstable = import nixpkgs-unstable {
        #  inherit system;
        #  config.allowUnfree = true;
        # };
      };

    ihpApp = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            tz
            wreq
            tagsoup
            modern-uri
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
        optimized = true;
    };

    in
    {
      packages.${system} = with nixpkgs.legacyPackages.${system}; {
        app = ihpApp;
        docker = dockerTools.buildImage {
         name = "badjunk";
         contents = [ cacert tzdata ];
         config = {
           Cmd = [ "${ihpApp}/bin/RunProdServer" ];
         };
        };
      };

      nixosConfigurations."badjunk" = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = attrs // {
          environment = "production";
        };
        modules = [
          # Overlays-module makes "pkgs.unstable" available in configuration.nix
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-unstable ]; })
          ./nixos/configuration.nix
        ];
      };
    };
}
    
