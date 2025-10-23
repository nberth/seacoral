############################################################################
##                                                                        ##
##  Copyright (c) 2025 OCamlPro                                           ##
##                                                                        ##
##  All rights reserved.                                                  ##
##  This file is distributed under the terms of the GNU General Public    ##
##  License version 3.                                                    ##
##                                                                        ##
############################################################################

# Creating build image. This image only installs the basic SeaCoral
# dependencies.
# This will not provide a useful seacoral instance though, as no useful tool is
# shipped with the orchestrator.
FROM debian:12-slim AS builder

COPY docker/scripts/hack-sudoers.sh .
RUN apt-get update                                                             \
 && apt-get install --yes sudo                                                 \
 && sh ./hack-sudoers.sh                                                       \
 && rm hack-sudoers.sh

################################################################################

# Creating the base image. It comes with libfuzzer, klee, and cbmc,
# but not seacoral itself
FROM builder AS base

# Install base dependencies for most external tools.
COPY scripts/base-depext.sh .
RUN sudo apt-get update                                                        \
 && sudo bash base-depext.sh                                                   \
 && sudo apt-get clean --yes                                                   \
 && rm base-depext.sh

# Install CBMC
ARG CBMC_VERSION=6.6.0
COPY scripts/install-cbmc.sh .
RUN bash install-cbmc.sh                                                       \
 && rm install-cbmc.sh

# Install Klee & libfuzzer
# Environment variables $CLANG and $CLANGXX are used by seacoral
ARG CLANG_VERSION=14
ENV CLANG=clang-$CLANG_VERSION                                                 \
    CLANGXX=clang++-$CLANG_VERSION                                             \
    LLVM=llvm-$CLANG_VERSION                                                   \
    LLVM_CONFIG=llvm-config-$CLANG_VERSION
COPY scripts/install-klee.sh .
RUN sudo apt-get update                                                        \
 && sudo apt-get install --yes $CLANG $LLVM $LLVM-dev                          \
 && sudo apt-get clean --yes                                                   \
 && bash install-klee.sh                                                       \
 && rm install-klee.sh

################################################################################

# Creating the executable image. It comes with base tools and seacoral itself
FROM base AS executable

# Create a dedicated user
RUN adduser --comment "Reef explorer" diver \
 && adduser --group divers \
 && adduser diver divers \
 && usermod --append --groups sudo diver \
 && echo >>/etc/sudoers "diver ALL = NOPASSWD:ALL"
USER diver
WORKDIR /home/diver

# Be sure to source `~/.profile` so we don't have to `eval $(opam
# env)` manually.
SHELL ["/bin/bash","-l","-c"]
CMD ["/bin/bash","-l"]

# Add opam
ADD https://github.com/ocaml/opam/releases/download/2.4.1/opam-2.4.1-x86_64-lin\
ux /usr/bin/opam
ARG OCAML_VERSION=4.14.2
RUN sudo chmod a+x /usr/bin/opam \
 && opam init --disable-sandboxing --compiler=$OCAML_VERSION --auto-setup

# Install the unforeseen dependencies of the project that are not
# defined in opam/*.opam.
COPY scripts/seacoral-depext.sh .
RUN sudo apt-get update                                                        \
 && sudo bash seacoral-depext.sh                                               \
 && sudo apt-get clean --yes                                                   \
 && rm seacoral-depext.sh

# Install seacoral dependencies (includes frama-c & needed plug-ins)
#
# Doing this before importing the whole project with the `COPY . .`
# below, makes docker reuse any cached image with all installed
# dependencies whenever none of `opam/*.opam` changes.
#
# If requested, also prepare to run tests
ARG RUN_TESTS=no
COPY opam ./opam
RUN if [ "$RUN_TESTS" = yes ];                                                 \
    then while read in;                                                        \
         do opam pin --yes --no-action add $in;                                \
         done < opam/docker-test-pins;                                         \
    fi
RUN opam install -y --deps-only opam/*.opam $(cat opam/runtime-deps)           \
 && opam clean
RUN if [ "$RUN_TESTS" = yes ];                                                 \
    then opam install -y --deps-only opam/*.opam --with-test                   \
      && opam clean;                                                           \
    fi
RUN opam install -y                         $(cat opam/runtime-deps)           \
 && opam clean                                                                 \
 && sudo rm -rf opam

# Configure Why3
RUN why3 config detect

# Setup some additional environment variables for LCOV reporting
ENV LLVM_COV=llvm-cov-$CLANG_VERSION                                           \
    LLVM_PROFDATA=llvm-profdata-$CLANG_VERSION

# Now that most runtime dependencies are available, we install a
# welcome message to great users that log into the distributed image.
COPY docker/WELCOME.md .
RUN sudo apt-get install --yes pandoc man                                      \
 && pandoc -s -f markdown -t      man WELCOME.md > WELCOME.man                 \
 && pandoc -s -f markdown -t asciidoc WELCOME.md > WELCOME.txt                 \
 && sudo apt-get purge --yes pandoc                                            \
 && sudo apt-get autopurge --yes                                               \
 && sudo apt-get clean --yes                                                   \
 && rm -f WELCOME.md
# Note: just putting a message into /etc/motd does not seem to work
# for SHELL and CMD above, so we do that by hand in ~/.bashrc
RUN exec >/tmp/motd                                                            \
 && echo "Welcome in Seacoral's Docker image!"                                 \
 && echo                                                                       \
 && echo "Run \`man -l ~/WELCOME.man\` for usage instructions"                 \
 && sudo mv /tmp/motd /etc/motd                                                \
 && echo "cat /etc/motd" >> ~/.bashrc

# Run tests (optional)
#
# We copy only what's necessary for the tool to build and tests to
# run.
#
# We artificially limit parallelism when running tests as many of the
# underlying tools are sensitive to CPU and memory availability.
ARG RUN_TESTS_JOBS=1
COPY --chown=diver:divers dune* Makefile* seacoral/
COPY --chown=diver:divers src seacoral/src/
COPY --chown=diver:divers test seacoral/test/
RUN if [ "$RUN_TESTS" = yes ];                                                 \
    then time make -C seacoral ci-tests CI_TESTS_JOBS=$RUN_TESTS_JOBS;         \
    fi

# Lastly, actually build and (optionally) install seacoral, and
# perform final (aggressive) cleanup (should keep seacoral/_build
# ... I guess ...)
#
# Here we copy the full source tree with its .git so the versioning
# machinery works properly.
#
# Still, we remove seacora/.git directory to accelerate opam pinning
# (which appears to copy the whole source tree)
ARG SKIP_INSTALL=no
COPY --chown=diver:divers . seacoral
RUN make -C seacoral                                                           \
 && if [ "$SKIP_INSTALL" != yes ];                                             \
    then rm -rf seacoral/.git                                                  \
      && OPAMINPLACEBUILD=true OPAMYES=true make -C seacoral opam              \
      && opam repo remove default --all --yes                                  \
      && opam clean --yes;                                                     \
    fi                                                                         \
 && rm -rf seacoral opam                                                       \
           $OPAM_SWITCH_PREFIX/.opam-switch/sources

# Lastly, create a work directory, and make it bindable with both read
# and write permssions via `--volume <hostdir>:/workdir`.  Also, hack
# a workaround to volume mounts (note user/group IDs may still be
# messed up on the host---user namespaces may be a way to solve this).
RUN sudo mkdir -p /workdir                                                     \
 && sudo chown -R diver:divers /workdir                                        \
 && echo "sudo chown diver:divers /workdir" >> ~/.bashrc
VOLUME /workdir
WORKDIR /workdir
