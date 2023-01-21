{ config, pkgs, modulesPath, lib, environment, ... }:
{
  imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
    ./ship.nix
    ./site.nix
  ];

  nix.settings.substituters = [
    "https://digitallyinduced.cachix.org"
  ];
  nix.settings.trusted-public-keys = [
    "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
  ];
  swapDevices = [{ device = "/swapfile"; size = 2048; }];

  # Add system-level packages for your server here
  environment.systemPackages = with pkgs; [
    bash
    jc
    direnv
  ];

  # Loads all environment variables into shell. Remove this if you don't want this enabled
  environment.shellInit = "set -o allexport; source /etc/shipnix/.env; set +o allexport";

  nix.settings.sandbox = false;

  # Automatic garbage collection. Enabling this frees up space by removing unused builds periodically
  nix.gc = {
    automatic = false;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 80 443 22 ];

  programs.vim.defaultEditor = true;

  services.fail2ban.enable = true;
  system.stateVersion = "22.11";
}

