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

- Do not force `:keep-eval nil` for normal level-2 delivery. Some Quicklisp or
  transport dependencies can leave interpreted callbacks in the image, and a
  stripped evaluator later fails as `SYSTEM::*%APPLY-INTERPRETED-FUNCTION*`.
  If experimenting with `DELIVERY_KEEP_EVAL=0`, keep
  `:error-on-interpreted-functions t` so this fails during delivery instead of
  daemon startup.

- Prefer adding narrow diagnostic CLI commands for delivered-image smoke tests
  instead of testing only through XMPP. For example, `xmpp-cli agent focus
  <code>` exercises the same tmux focus path as an XMPP bare-code reply.

## Agent Command Conventions

- Use `define-route-command` for daemon XMPP commands that should exist in both
  direct chat and room chat. Keep the command body as normal Lisp code; the
  macro should only own command registration, route or room target resolution,
  usage text, and direct-vs-room reply routing.

- Room chat commands get their route context from the room binding. Direct chat
  commands parse the route code from the first argument unless the command has
  no additional positional arguments and explicitly uses `:direct-route
  :optional`.

- If a command accepts positional arguments beyond the route code, keep
  `:direct-route :required` so direct chat stays unambiguous:
  `/cmd <route-code> arg1...`. The route code is always the first direct-chat
  argument.

- Use `:direct-name :same` when direct and room command names match, such as
  `/focus`. Use `:direct-name :room-prefixed` when the direct command should be
  namespaced while the room command is local, such as direct `/room-close
  <route-code>` versus room `/close`.

- Choose `:target :route` for commands operating on a tmux route and
  `:target :room` for commands operating on the room bound to a route. Room
  lifecycle commands should not require the underlying tmux route to still be
  live unless that is part of the command's behavior.

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
   temporary `XMPP_CLI_DATA_DIR`. Do not rely on `HOME` for delivered
   LispWorks images; `user-homedir-pathname` may still resolve to the real
   account home.

   ```sh
   tmp_data=$(mktemp -d)
   printf '%s' '{"hook_event_name":"Stop","model":"smoke","turn_id":"unicode","cwd":"/tmp","last_assistant_message":"I\u2019m fine"}' \
     | XMPP_CLI_DATA_DIR="$tmp_data" ./build/xmpp-cli agent notify-codex
   rm -rf "$tmp_data"
   ```

5. If the daemon is running, restart it after rebuilding:

   ```sh
   ./build/xmpp-cli agent stop || true
   setsid -f ./build/xmpp-cli agent daemon --foreground \
     >> "$HOME/.local/xmpp-cli/agent/daemon.log" 2>&1
   ./build/xmpp-cli agent status
   ```
