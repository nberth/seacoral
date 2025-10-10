#!/bin/sh
resdir="/tmp/results"
mkdir -p "${resdir}" && \
    tar --extract --directory "${resdir}" >&2 && \
    ./venv/bin/python3 ./klee-evals.py "$@" --output-dir "${resdir}" >&2 && \
    tar --create  --directory "${resdir}" --dereference .
