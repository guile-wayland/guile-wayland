#!/usr/bin/env bash
guix shell -D  -f guix.scm -- sh -c './bootstrap && ./configure && make -j'
guix shell -- ./pre-inst-env guile --listen
