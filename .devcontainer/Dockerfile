FROM ubuntu:22.04

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

 
 # Install dependencies to add package sources
RUN apt-get update \
     && apt-get install -y software-properties-common curl gnupg pkg-config g++ make libgmp-dev git ssh libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev\
     && apt-get clean \
     && rm -rf /var/lib/apt/lists/*
 
# Create the vscode user.
ARG USERNAME=vscode
RUN adduser --disabled-password --gecos '' ${USERNAME}
USER "${USERNAME}"
 
# Install minimal GHCup.
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=True
ENV BOOTSTRAP_HASKELL_MINIMAL=True
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
 
# Update PATH to include ghcup
ENV PATH="/home/${USERNAME}/.ghcup/bin:/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:${PATH}"

# Install GHC and related tools.
RUN ghcup install cabal 3.10.3.0 \
     && ghcup install ghc --set 9.4.8 \
     && ghcup install hls 2.9.0.1
 
# Make sure cabal's package database is up to date.
RUN cabal update
 
# Set shell to bash to use auto completion (e.g. arrow up for last command).
ENV SHELL="/bin/bash"
