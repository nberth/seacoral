#!/bin/sh
export DEBIAN_FRONTEND=noninteractive
set -o xtrace

# Required environment: CLANG, LLVM_CONFIG

dir="$(mktemp --directory)"
trap 'rm -rf -- "${dir}"' EXIT
cd "${dir}"

# Checkout the code
git clone https://github.com/klee/klee.git --depth 1 --branch v3.1
git clone https://github.com/klee/klee-uclibc.git --depth 1
git clone https://github.com/stp/stp --depth 1 --branch 2.3.3

# Apply symbolic memory size patch
( cd klee &&
      curl -L https://github.com/klee/klee/pull/1722.diff | patch -p1 )

# Build & installl STP
mkdir stp/build
( cd stp/build &&
      cmake .. &&
      make -j5 &&
      sudo make install &&
      make clean )

# Build & install uClibc
( cd klee-uclibc &&
      ./configure --make-llvm-lib		     \
		  --with-cc $CLANG		     \
		  --with-llvm-config $LLVM_CONFIG &&
      make -j5 )

# Build & install Klee
mkdir klee/build
( cd klee/build &&
      cmake -DENABLE_SOLVER_STP=ON			 \
	    -DENABLE_SOLVER_Z3=ON			 \
	    -DENABLE_POSIX_RUNTIME=ON			 \
	    -DENABLE_UNIT_TESTS=OFF			 \
	    -DENABLE_SYSTEM_TESTS=OFF			 \
	    -DENABLE_KLEE_LIBCXX=OFF			 \
	    -DENABLE_KLEE_EH_CXX=OFF			 \
	    -DKLEE_UCLIBC_PATH="$dir/klee-uclibc"	 \
	    -DLLVM_CONFIG_BINARY="/usr/bin/$LLVM_CONFIG" \
	    .. &&
      make -j5 &&
      sudo make install &&
      make clean )
