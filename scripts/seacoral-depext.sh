#!/bin/sh
export DEBIAN_FRONTEND=noninteractive
apt-get install -y \
	autoconf \
	libgmp-dev \
	pkg-config \
	time \
	lcov
