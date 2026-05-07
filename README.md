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

`build/deliver.lisp` loads Quicklisp at build time, registers this project and `vendor/cl-xmpp`, quickloads `xmpp-cli`, and delivers at level `0`. The delivered binary should not need Quicklisp at runtime.

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

`send -f` does not send a binary attachment. True file upload is a future feature and should use XEP-0363 HTTP File Upload.

## State

Local state is stored under:

```text
$HOME/.local/xmpp-cli/
```

Files:

```text
config.sexp
history.sexp
logs/
```

The implementation makes a best-effort attempt to set the state directory to mode `0700` and state files to mode `0600` on Unix-like systems. History stores only metadata, byte counts, SHA-256 hashes, and send results. It does not store message bodies.

Use an app-specific XMPP password when your server supports one.

## Backend Notes

The current `cl-xmpp` backend supports `SCRAM-SHA-1`, `PLAIN`, and
`DIGEST-MD5`. `SCRAM-SHA-1-PLUS` is not enabled yet because it requires TLS
channel binding data from the TLS stream; the current `cl+ssl` integration
does not expose that data to this backend.

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
