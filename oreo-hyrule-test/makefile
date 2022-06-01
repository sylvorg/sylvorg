.RECIPEPREFIX := |
.DEFAULT_GOAL := test

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))

test:
|nix-shell

rest:
|nix-shell --option tarball-ttl 0