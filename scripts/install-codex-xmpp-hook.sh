#!/bin/sh
set -eu
umask 077

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
CODEX_HOME="${CODEX_HOME:-$HOME/.codex}"
TARGET="${1:-${CODEX_XMPP_NOTIFY_TO:-}}"
HOOK_SRC="$ROOT/codex-hooks/xmpp-notify.py"
HOOK_DIR="$CODEX_HOME/hooks"
HOOK_DEST="$HOOK_DIR/xmpp-notify.py"
ENV_DEST="$HOOK_DIR/xmpp-notify.env"
CONFIG="$CODEX_HOME/config.toml"

PYTHON_BIN="$(command -v python3 || true)"
if [ "$#" -gt 1 ] || [ -z "$TARGET" ]; then
  printf 'Usage: %s user@example.org\n' "$0" >&2
  printf '   or: CODEX_XMPP_NOTIFY_TO=user@example.org %s\n' "$0" >&2
  exit 2
fi

case "$TARGET" in
  *'
'*)
    printf 'XMPP notification recipient must be a single line\n' >&2
    exit 2
    ;;
esac

if [ -z "$PYTHON_BIN" ]; then
  printf 'python3 is required to install and run the Codex XMPP hook\n' >&2
  exit 1
fi

if ! command -v xmpp-cli >/dev/null 2>&1; then
  printf 'warning: xmpp-cli is not currently in PATH; install it before Codex runs the hook\n' >&2
fi

mkdir -p "$HOOK_DIR"
install -m 700 "$HOOK_SRC" "$HOOK_DEST"
tmp_env="$ENV_DEST.$$"
printf 'CODEX_XMPP_NOTIFY_TO=%s\n' "$TARGET" > "$tmp_env"
chmod 600 "$tmp_env"
mv "$tmp_env" "$ENV_DEST"
touch "$CONFIG"

export CONFIG HOOK_DEST PYTHON_BIN
python3 <<'PY'
import json
import os
import re
import shlex
from pathlib import Path


config_path = Path(os.environ["CONFIG"])
hook_dest = os.environ["HOOK_DEST"]
python_bin = os.environ["PYTHON_BIN"]
command = f"{shlex.quote(python_bin)} {shlex.quote(hook_dest)}"

start_marker = "# BEGIN xmpp-cli Codex XMPP hook"
end_marker = "# END xmpp-cli Codex XMPP hook"
hook_block = f"""
{start_marker}
[[hooks.Stop]]

[[hooks.Stop.hooks]]
type = "command"
command = {json.dumps(command)}
timeout = 20
statusMessage = "Sending XMPP completion notice"

[[hooks.PermissionRequest]]

[[hooks.PermissionRequest.hooks]]
type = "command"
command = {json.dumps(command)}
timeout = 20
statusMessage = "Sending XMPP input notice"
{end_marker}
""".strip() + "\n"


def ensure_feature(text):
    lines = text.splitlines(keepends=True)
    feature_header = re.compile(r"^\s*\[features\]\s*(?:#.*)?$")
    section_header = re.compile(r"^\s*\[")
    codex_hooks = re.compile(r"^(\s*)codex_hooks\s*=.*$")

    for index, line in enumerate(lines):
        if not feature_header.match(line):
            continue

        section_end = len(lines)
        for next_index in range(index + 1, len(lines)):
            if section_header.match(lines[next_index]):
                section_end = next_index
                break

        for hook_index in range(index + 1, section_end):
            if codex_hooks.match(lines[hook_index]):
                lines[hook_index] = "codex_hooks = true\n"
                return "".join(lines)

        lines.insert(index + 1, "codex_hooks = true\n")
        return "".join(lines)

    insert_at = len(lines)
    for index, line in enumerate(lines):
        if section_header.match(line):
            insert_at = index
            break

    prefix = ["[features]\n", "codex_hooks = true\n", "\n"]
    lines[insert_at:insert_at] = prefix
    return "".join(lines)


def replace_hook_block(text):
    pattern = re.compile(
        rf"\n?{re.escape(start_marker)}\n.*?{re.escape(end_marker)}\n?",
        re.DOTALL,
    )
    text = pattern.sub("\n", text).rstrip() + "\n\n"
    return text + hook_block


current = config_path.read_text(encoding="utf-8") if config_path.exists() else ""
updated = replace_hook_block(ensure_feature(current))
config_path.write_text(updated, encoding="utf-8")
PY

if command -v codex >/dev/null 2>&1; then
  codex debug prompt-input "codex xmpp hook config check" >/dev/null
fi

printf 'Installed Codex XMPP hook to %s\n' "$HOOK_DEST"
printf 'Wrote private hook config to %s\n' "$ENV_DEST"
printf 'Updated Codex config at %s\n' "$CONFIG"
