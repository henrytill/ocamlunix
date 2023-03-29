FROM mcr.microsoft.com/devcontainers/base:bullseye

ARG DEFAULT_SWITCH=4.14.1

ENV DEBIAN_FRONTEND=noninteractive

# Install OCaml and OPAM packages
RUN apt-get update -qq && \
    apt-get install -yqq ocaml opam pkg-config

# Switch to the vscode user
USER vscode

# Initialize OPAM
RUN opam init --disable-sandboxing --auto-setup -q && \
    opam switch create $DEFAULT_SWITCH && \
    opam switch set $DEFAULT_SWITCH && \
    eval $(opam env)

# Install ocaml-lsp-server, ocamlformat, and dune
RUN opam install -yq ocaml-lsp-server ocamlformat dune depext
