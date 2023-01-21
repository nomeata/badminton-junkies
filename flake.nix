{
  description = "Shipnix server configuration for badjunk";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable } @attrs:
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
    in
    {
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
    