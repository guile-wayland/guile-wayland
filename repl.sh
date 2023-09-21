#!/usr/bin/env bash

guix shell guile -f guix.scm --rebuild-cache -- guile --listen=1337
