#!/usr/bin/env bash

nixos-rebuild switch \
    --use-substitutes \
    --target-host root@badjunk.nomeata.de \
    --build-host root@badjunk.nomeata.de \
    --flake .#badjunk \
    --fast \
    --option extra-substituters "https://digitallyinduced.cachix.org" \
    --option extra-trusted-public-keys "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="

ssh root@badjunk.nomeata.de systemctl start migrate
