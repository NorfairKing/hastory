#!/usr/bin/env bash

set -ex

stack install :hastory-server --file-watch --exec='./scripts/redo.sh'
