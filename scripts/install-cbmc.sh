#!/bin/sh
export DEBIAN_FRONTEND=noninteractive
set -o xtrace

# Required environment: CBMC_VERSION

CBMC_DEB_FILE="ubuntu-22.04-cbmc-$CBMC_VERSION-Linux.deb"
BASE_URL="https://github.com/diffblue/cbmc/releases/download"

deb="$(mktemp --suffix .deb)"
trap 'rm -f -- "${deb}"' EXIT

curl --location "$BASE_URL/cbmc-$CBMC_VERSION/$CBMC_DEB_FILE" --output "${deb}"
sudo apt-get install --yes "${deb}"
