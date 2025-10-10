#!/bin/sh
export DEBIAN_FRONTEND=noninteractive

# opam
apt-get install --yes \
    build-essential \
    git \
    rsync \
    curl \
    unzip

# Most tools
apt-get install -y \
    perl \
    autoconf \
    cmake \
    libsqlite3-dev \
    zlib1g-dev

# Frama-C deps
apt-get install -y \
    graphviz \
    libgtksourceview-3.0-dev

# STP
apt-get install -y \
    bison \
    flex \
    libboost-all-dev \
    dh-python \
    perl \
    minisat

# Klee
apt-get install -y \
    libgoogle-perftools-dev \
    z3
