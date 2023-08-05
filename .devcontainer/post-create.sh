#!/usr/bin/env bash

# This script is run after the project is created.

opam install -ty --deps . --depext
opam install -ty --deps .
