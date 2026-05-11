#!/bin/sh
set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
mkdir -p "$ROOT/build"

OUTPUT="$ROOT/build/xmpp-cli"
TMP_OUTPUT="$ROOT/build/xmpp-cli.new.$$"

cleanup() {
  rm -f "$TMP_OUTPUT"
}

trap cleanup EXIT HUP INT TERM

XMPP_CLI_DELIVERY_OUTPUT="$TMP_OUTPUT" lw-console -build "$ROOT/build/deliver.lisp"
chmod 755 "$TMP_OUTPUT"
mv -f "$TMP_OUTPUT" "$OUTPUT"
trap - EXIT HUP INT TERM
