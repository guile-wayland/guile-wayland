#!/usr/bin/env bash
guix shell -D  -f guix.scm -- sh -c './bootstrap && ./configure && make -j'
guix shell -D  -f guix.scm -- ./pre-inst-env guile --listen
