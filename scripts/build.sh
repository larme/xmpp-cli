#!/bin/sh
set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
mkdir -p "$ROOT/build"

exec lw-console -build "$ROOT/build/deliver.lisp"
