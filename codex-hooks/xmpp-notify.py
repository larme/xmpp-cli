#!/usr/bin/env python3
import datetime as _datetime
import json
import os
import shutil
import socket
import subprocess
import sys
import traceback


HOME = os.path.expanduser("~")
MAX_DETAIL_CHARS = 1600
SEND_TIMEOUT_SECONDS = 15


def hook_dir():
    return os.path.dirname(os.path.abspath(__file__))


def log(message):
    timestamp = _datetime.datetime.now().isoformat(timespec="seconds")
    log_path = os.path.join(hook_dir(), "xmpp-notify.log")
    try:
        os.makedirs(os.path.dirname(log_path), mode=0o700, exist_ok=True)
        with open(log_path, "a", encoding="utf-8") as handle:
            handle.write(f"{timestamp} {message}\n")
    except Exception:
        pass


def rel_home(path):
    if not path:
        return "unknown"
    path = os.path.abspath(path)
    if path == HOME:
        return "~"
    prefix = HOME + os.sep
    if path.startswith(prefix):
        return "~/" + path[len(prefix):]
    return path


def git_root(cwd):
    if not cwd:
        return None
    try:
        result = subprocess.run(
            ["git", "-C", cwd, "rev-parse", "--show-toplevel"],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=2,
            check=True,
        )
    except Exception:
        return None
    root = result.stdout.strip()
    return root or None


def trim(text, limit=MAX_DETAIL_CHARS):
    text = "" if text is None else str(text).strip()
    if len(text) <= limit:
        return text
    return text[: limit - 1].rstrip() + "..."


def compact_json(value):
    try:
        return json.dumps(value, ensure_ascii=False, separators=(",", ":"))
    except Exception:
        return str(value)


def summarize_tool_input(payload):
    tool_input = payload.get("tool_input")
    if isinstance(tool_input, dict):
        description = tool_input.get("description")
        command = tool_input.get("command")
        if description and command:
            return trim(f"{description}\n{command}")
        if command:
            return trim(command)
        if description:
            return trim(description)
    return trim(compact_json(tool_input))


def payload_value(payload, key, default="unknown"):
    value = payload.get(key)
    if value is None or value == "":
        return default
    return str(value)


def target_from_env_file():
    env_path = os.path.join(hook_dir(), "xmpp-notify.env")
    try:
        with open(env_path, "r", encoding="utf-8") as handle:
            for raw_line in handle:
                line = raw_line.strip()
                if not line or line.startswith("#") or "=" not in line:
                    continue
                key, value = line.split("=", 1)
                if key.strip() != "CODEX_XMPP_NOTIFY_TO":
                    continue
                value = value.strip()
                if len(value) >= 2 and value[0] == value[-1] and value[0] in "'\"":
                    value = value[1:-1]
                return value or None
    except FileNotFoundError:
        return None
    except Exception as exc:
        log(f"failed to read xmpp-notify.env: {exc}")
        return None


def target_jid():
    return os.environ.get("CODEX_XMPP_NOTIFY_TO") or target_from_env_file()


def header(event, payload):
    host = socket.gethostname().split(".", 1)[0]
    cwd = os.path.abspath(payload.get("cwd") or os.getcwd())
    repo = git_root(cwd) or cwd
    parts = [
        event,
        f"host={host}",
        f"repo={rel_home(repo)}",
        f"cwd={rel_home(cwd)}",
    ]

    tool_name = payload.get("tool_name")
    if tool_name:
        parts.append(f"tool={tool_name}")

    return " | ".join(parts)


def build_message(payload):
    event = payload_value(payload, "hook_event_name", "Codex")
    model = payload_value(payload, "model")
    turn_id = payload_value(payload, "turn_id", "none")

    common = [
        f"model: {model}",
        f"turn: {turn_id}",
    ]

    if event == "Stop":
        last_message = trim(payload.get("last_assistant_message"), 1800)
        lines = [header("Codex finished", payload), *common]
        if last_message:
            lines.extend(["", last_message])
        return "\n".join(lines)

    if event == "PermissionRequest":
        permission_mode = payload_value(payload, "permission_mode")
        detail = summarize_tool_input(payload)
        lines = [
            header("Codex needs input", payload),
            *common,
            f"permission: {permission_mode}",
        ]
        if detail:
            lines.extend(["", detail])
        return "\n".join(lines)

    return "\n".join([header(f"Codex hook: {event}", payload), *common])


def send_xmpp(message):
    target = target_jid()
    if not target:
        log("no XMPP notification target configured; skipping XMPP notification")
        return

    xmpp_cli = shutil.which("xmpp-cli") or "/usr/local/bin/xmpp-cli"
    subprocess.run(
        [xmpp_cli, "send", target, message],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        timeout=SEND_TIMEOUT_SECONDS,
        check=True,
    )


def main():
    raw = sys.stdin.read()
    try:
        payload = json.loads(raw) if raw.strip() else {}
    except Exception as exc:
        log(f"failed to parse hook payload: {exc}")
        return 0

    try:
        send_xmpp(build_message(payload))
    except Exception as exc:
        log(f"failed to send XMPP notification: {exc}")
        log(traceback.format_exc().rstrip())

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
