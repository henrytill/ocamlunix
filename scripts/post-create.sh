#!/usr/bin/env bash

# This script is run after the project is created.

# https://github.com/ocaml/opam/issues/3790#issuecomment-765650349
opam install . --dry-run --deps-only -ty | awk '/-> installed/{print $3}' | xargs opam depext -iy
