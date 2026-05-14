# xmpp-cli

`xmpp-cli` is a small Common Lisp command-line sender for XMPP accounts. The MVP targets LispWorks delivery and uses `clingon` for command parsing plus the vendored `cl-xmpp` backend with TLS/SASL support.

## Build

Quicklisp is expected to be installed. The build script assumes the LispWorks console executable is named `lw-console` and is available in `PATH`.

```sh
./scripts/build.sh
```

The delivered binary is written to:

```text
build/xmpp-cli
```

`build/deliver.lisp` loads Quicklisp at build time, registers this project and `vendor/cl-xmpp`, quickloads `xmpp-cli`, and delivers at level `2` by default. The delivered binary should not need Quicklisp at runtime.

Use `DELIVERY_LEVEL` to tune LispWorks delivery size versus risk while testing:

```sh
DELIVERY_LEVEL=0 ./scripts/build.sh   # largest, keeps delivered debug support by default
DELIVERY_LEVEL=3 ./scripts/build.sh   # smaller, needs runtime testing
```

Set `DELIVERY_DEBUG=1` when you need LispWorks delivered-image debugger support at a higher delivery level, or `DELIVERY_DEBUG=0` to force it off for a level `0` build. The delivery script keeps evaluator support by default below delivery level `4`, matching LispWorks' safer default for images that may retain interpreted callbacks from dependencies. Set `DELIVERY_KEEP_EVAL=0` only for aggressive size experiments; the build will fail if interpreted functions would remain. The script also keeps LispWorks reader and pretty-printer support because the daemon IPC uses a small local S-expression protocol and XMPP error paths may print XML objects. Do not run `strip` on the delivered executable; it removes the LispWorks image trailer and corrupts the binary.

## Usage

Store a verified login profile. Passwords are read from stdin so they are not exposed through shell history or process listings.

```sh
printf '%s\n' 'secret' | ./build/xmpp-cli login user@example.org --password-stdin
```

By default `login` uses `--mechanism auto`, which prefers `SCRAM-SHA-1`
when the server advertises it and falls back to `PLAIN` or `DIGEST-MD5`.
You can force a mechanism when needed:

```sh
printf '%s\n' 'secret' | ./build/xmpp-cli login user@example.org --password-stdin --mechanism scram-sha-1
```

Send literal text:

```sh
./build/xmpp-cli send friend@example.org "hello from xmpp-cli"
```

Send UTF-8 text file contents as the message body:

```sh
./build/xmpp-cli send friend@example.org -f ./message.txt
```

When the agent daemon is running, `send` first asks the daemon to send through
its persistent XMPP connection. If the daemon is absent, stale, or not
connected, `send` falls back to the standalone one-shot connection.

`send -f` does not send a binary attachment. True file upload is a future feature and should use XEP-0363 HTTP File Upload.

## Agent Daemon

Configure the notification and reply account:

```sh
./build/xmpp-cli agent config set-notify-to larme@example.org
```

`set-notify-to` also allows that JID as a reply sender when the sender list is
empty. Additional senders can be managed with:

```sh
./build/xmpp-cli agent config allow-sender other@example.org
./build/xmpp-cli agent config remove-sender other@example.org
```

Start the daemon in a tmux window or a service wrapper:

```sh
./build/xmpp-cli agent daemon --foreground
```

Check and stop it with:

```sh
./build/xmpp-cli agent status
./build/xmpp-cli agent stop
```

Discover the server's MUC service when you want chat rooms:

```sh
./build/xmpp-cli agent discover-muc
./build/xmpp-cli agent discover-muc --force
```

Codex notifications include a four-letter lowercase route code. Reply with
only the code to focus that tmux pane:

```text
abcd
```

Reply with the code plus text to focus the pane, paste the text into the
agent, and press Enter:

```text
abcd please rerun the failing test
```

## State

Local state is stored under:

```text
$HOME/.local/xmpp-cli/
```

Files:

```text
config.yaml
history.yaml
agent/config.yaml
agent/routes.yaml
agent/control.yaml
agent/tmp/
agent/muc-services.yaml
agent/rooms.yaml
logs/
```

The implementation makes a best-effort attempt to set the state directory to mode `0700` and state files to mode `0600` on Unix-like systems. History stores only metadata, byte counts, SHA-256 hashes, and send results. It does not store message bodies.

The state directory is local machine state and should not be shared across
machines or network filesystems. For multi-machine agent use, configure a
separate XMPP account and local state directory on each machine.

Use an app-specific XMPP password when your server supports one.

## Backend Notes

The current `cl-xmpp` backend supports `SCRAM-SHA-1`, `PLAIN`, and
`DIGEST-MD5`. `SCRAM-SHA-1-PLUS` is not enabled yet because it requires TLS
channel binding data from the TLS stream; the current `cl+ssl` integration
does not expose that data to this backend.

## Codex XMPP Hook

This repository includes an installable Codex hook that sends XMPP
notifications when Codex finishes a turn or needs human approval. It assumes
`xmpp-cli` is installed in `PATH` and already has a usable default profile.

Configure the notification recipient once:

```sh
xmpp-cli agent config set-notify-to you@example.org
```

Then install or update the Codex hook:

```sh
./scripts/install-codex-xmpp-hook.sh
```

For convenience, the installer can still set the recipient while installing:

```sh
./scripts/install-codex-xmpp-hook.sh you@example.org
CODEX_XMPP_NOTIFY_TO=you@example.org ./scripts/install-codex-xmpp-hook.sh
```

The installer enables Codex hooks and registers `Stop` plus
`PermissionRequest` hooks in `config.toml` using:

```sh
xmpp-cli agent notify-codex
```

Notification headers include the route code, event, hostname, repository
location, current working directory, and tool name when Codex is waiting for
approval.

When replying from XMPP, prefix a route code to target a specific Codex pane.
If the message does not start with an active route code, the daemon sends it to
the most recently active route.

Messages beginning with `/` are treated as commands. `/focus [route-code]`
focuses the tmux pane for the selected route. `/new [route-code]` starts a
fresh Codex session in a new tmux window, using the same working directory and
tmux session as the selected route. When `route-code` is omitted, the most
recently active route is used. The daemon reply includes the new route code for
the fresh window.

Room commands use XEP-0045 MUC rooms for route-specific feedback:

```text
/room <route-code> [room-name]
/rooms
/room-close <route-code>
```

`/room` always requires an explicit route code. It creates a temporary private
room, binds it to that route, invites the command sender, and replies with the
room JID plus `xmpp:...?...join` URI. Messages sent inside the room are routed
to the bound tmux pane without changing the direct-chat default route. Inside a
room, room-local commands omit the `room-` prefix, so `/room-close ...` in a
direct chat becomes `/close` in the room itself. `/focus` also works inside a
room and focuses the pane bound to that room.

## Tests

The test system covers JID parsing, config round trips, history metadata, CLI validation, and CLI send behavior through a fake backend.

```sh
sbcl --load ~/.local/quicklisp/setup.lisp \
  --eval '(push #P"/path/to/xmpp-cli/" asdf:*central-registry*)' \
  --eval '(push #P"/path/to/xmpp-cli/vendor/cl-xmpp/" asdf:*central-registry*)' \
  --eval '(ql:quickload "xmpp-cli/test")' \
  --eval '(asdf:test-system "xmpp-cli/test")' \
  --quit
```

Manual XMPP tests require a real XMPP account:

```sh
printf '%s\n' 'secret' | ./build/xmpp-cli login user@example.org --password-stdin
./build/xmpp-cli send friend@example.org "hello from xmpp-cli"
./build/xmpp-cli send friend@example.org -f ./message.txt
```
