# Project Notes For Agents

This project targets LispWorks delivery. Do not assume behavior seen under
SBCL, Quicklisp, or an interactive LispWorks image is enough. Every change that
touches process execution, streams, Unicode text, hooks, tmux, daemon IPC, or
XMPP transport should be checked in the delivered `build/xmpp-cli` image.

## LispWorks Delivery Lessons

- Run both the test system and a delivered-image smoke test. The normal test
  command catches portable behavior, but delivery changes what is retained in
  the image and can expose different stream and subprocess behavior.

- Use character streams explicitly for user or hook text. In delivered
  LispWorks, default string output streams may be `BASE-CHAR`, which cannot
  hold characters such as `U+2019`. When using `with-output-to-string` for JSON,
  YAML, XMPP messages, hook payloads, or file contents, prefer:

  ```lisp
  (with-output-to-string (out nil :element-type 'character)
    ...)
  ```

- Common Lisp strings do not use C-style escapes such as `"\t"`. That string
  contains `t`, not a tab. Use `#\Tab`, `write-char`, or a helper that joins
  strings with real character objects.

- Be cautious with `uiop:run-program` in delivered LispWorks images. It can
  depend on implementation internals that delivery may remove. Keep subprocess
  launching behind a small boundary, and use LispWorks APIs there when needed.

- When invoking external executables from a delivered image, resolve command
  names such as `tmux` to absolute paths first. LispWorks process APIs may not
  search `PATH` in the same way a shell does.

- If local IPC depends on reading or printing Lisp objects, keep the Lisp reader
  and pretty-printer during delivery. This repository's delivery script already
  keeps them because daemon control messages use a small local S-expression
  protocol.

- Prefer adding narrow diagnostic CLI commands for delivered-image smoke tests
  instead of testing only through XMPP. For example, `xmpp-cli agent focus
  <code>` exercises the same tmux focus path as an XMPP bare-code reply.

## Verification Checklist

Before calling a LispWorks-sensitive change done:

1. Run the portable test system:

   ```sh
   sbcl --noinform --non-interactive \
     --eval '(ql:quickload "xmpp-cli/test" :silent t)' \
     --eval '(asdf:test-system "xmpp-cli/test")'
   ```

2. Rebuild the delivered image:

   ```sh
   ./scripts/build.sh
   ```

3. Smoke test the delivered binary for the behavior changed. Useful examples:

   ```sh
   ./build/xmpp-cli agent status
   ./build/xmpp-cli agent focus <route-code>
   ```

4. For hook parsing, test Unicode without touching the real account by using a
   temporary `HOME`:

   ```sh
   tmp_home=$(mktemp -d)
   printf '%s' '{"hook_event_name":"Stop","model":"smoke","turn_id":"unicode","cwd":"/tmp","last_assistant_message":"I\u2019m fine"}' \
     | HOME="$tmp_home" ./build/xmpp-cli agent notify-codex
   rm -rf "$tmp_home"
   ```

5. If the daemon is running, restart it after rebuilding:

   ```sh
   ./build/xmpp-cli agent stop || true
   setsid -f ./build/xmpp-cli agent daemon --foreground \
     >> "$HOME/.local/xmpp-cli/agent/daemon.log" 2>&1
   ./build/xmpp-cli agent status
   ```

