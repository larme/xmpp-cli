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
MAX_MESSAGE_CHARS = 1600
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


def normalize(text):
    return "" if text is None else str(text).strip()


def join_message(prefix_lines, detail):
    lines = list(prefix_lines)
    if detail:
        lines.extend(["", detail])
    return "\n".join(lines)


def split_text(text, limit):
    chunks = []
    start = 0
    while start < len(text):
        end = min(len(text), start + limit)
        if end < len(text):
            newline = text.rfind("\n", start, end)
            space = text.rfind(" ", start, end)
            boundary = max(newline, space)
            if boundary >= start + limit // 2:
                end = boundary + 1
        chunks.append(text[start:end])
        start = end
    return chunks


def split_message(prefix_lines, detail, limit=MAX_MESSAGE_CHARS):
    message = join_message(prefix_lines, detail)
    if len(message) <= limit or not detail:
        return [message]

    total = 1
    while True:
        overhead = len("\n".join([f"[{total}/{total}]", *prefix_lines, "", ""]))
        chunk_limit = max(1, limit - overhead)
        chunks = split_text(detail, chunk_limit)
        next_total = len(chunks)
        if next_total == total:
            return [
                join_message([f"[{index}/{total}]", *prefix_lines], chunk)
                for index, chunk in enumerate(chunks, 1)
            ]
        total = next_total


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
            return normalize(f"{description}\n{command}")
        if command:
            return normalize(command)
        if description:
            return normalize(description)
    return normalize(compact_json(tool_input))


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


def build_message_parts(payload):
    event = payload_value(payload, "hook_event_name", "Codex")
    model = payload_value(payload, "model")
    turn_id = payload_value(payload, "turn_id", "none")

    common = [
        f"model: {model}",
        f"turn: {turn_id}",
    ]

    if event == "Stop":
        last_message = normalize(payload.get("last_assistant_message"))
        return [header("Codex finished", payload), *common], last_message

    if event == "PermissionRequest":
        permission_mode = payload_value(payload, "permission_mode")
        detail = summarize_tool_input(payload)
        return [
            header("Codex needs input", payload),
            *common,
            f"permission: {permission_mode}",
        ], detail

    return [header(f"Codex hook: {event}", payload), *common], ""


def build_message(payload):
    prefix_lines, detail = build_message_parts(payload)
    return join_message(prefix_lines, detail)


def build_messages(payload):
    prefix_lines, detail = build_message_parts(payload)
    return split_message(prefix_lines, detail)


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
        for message in build_messages(payload):
            send_xmpp(message)
    except Exception as exc:
        log(f"failed to send XMPP notification: {exc}")
        log(traceback.format_exc().rstrip())

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
