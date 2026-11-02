#!/usr/bin/env bash

nixos-rebuild switch \
    --use-substitutes \
    --target-host root@badjunk.nomeata.de \
    --build-host root@badjunk.nomeata.de \
    --flake .#badjunk \
    --fast \
    --option substituters "https://cache.nixos.org/" \
    --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" \
    --option extra-substituters "https://cache.digitallyinduced.com/public" \
    --option extra-trusted-public-keys "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE= public:kR6JCoqAIMaO4s+EdDGh+jsHEHnoLq4ZLJPMCo0hcIQ="

ssh root@badjunk.nomeata.de systemctl start migrate
