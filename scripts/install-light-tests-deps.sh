#!/bin/sh
export DEBIAN_FRONTEND=noninteractive

# Only clang for light tests
apt-get install -y \
    clang
