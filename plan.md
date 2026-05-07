# `xmpp-cli` Implementation Plan

This plan is intended for a coding agent such as Codex. Implement a Common Lisp command-line program named `xmpp-cli` that runs on LispWorks, logs in to an XMPP account, stores local auth/profile data under `$HOME/.local/xmpp-cli/`, and sends messages to arbitrary XMPP JIDs from the command line.

## 1. Goals and constraints

### Required behavior

- Implement in Common Lisp for LispWorks.
- Build using a script that assumes the LispWorks console binary is named `lw-console` and is available in `$PATH`.
- Deliver a single executable binary named `xmpp-cli`.
- Persist auth/profile data and send history in `$HOME/.local/xmpp-cli/`.
- Prefer pure Common Lisp dependencies. If a C library is eventually required, it should be buildable locally and bundled with the delivered artifact.
- Provide these user-facing commands:

```sh
xmpp-cli login ...
xmpp-cli send <account> "xxx"
xmpp-cli send <account> -f file
```

Here, `<account>` in `send` means the recipient XMPP account/JID, for example `friend@example.org`.

### MVP interpretation

- `send <account> -f file` means: read the local text file and send its contents as the XMPP message body.
- Do not implement real binary/file attachments in the MVP. True XMPP file upload should be a later feature using XEP-0363 HTTP File Upload, which requires requesting an upload slot, uploading bytes over HTTP PUT, and sending the resulting download URL.
- Do not add a `--password` command-line option. Passwords in argv can leak through shell history and process listings.
- Support `--password-stdin` for the MVP. An interactive hidden prompt can be added later.
- Use the domain part of the login JID as the default XMPP host unless `--host` is supplied.
- Do not implement DNS SRV lookup in the MVP unless it is already trivial with the selected libraries. Add it as a later enhancement.

## 2. Recommended dependency strategy

### CLI parser

Use `clingon` for command parsing.

Reasons:

- It is a Common Lisp command-line parser.
- It supports subcommands, short and long options, generated help, and version/help flags.
- Its ASDF dependencies are Lisp libraries: `uiop`, `bobbin`, `cl-reexport`, `split-sequence`, and `with-user-abort`.

### XMPP backend

Start with a small backend abstraction and implement the first backend using `cl-xmpp`.

Reasons:

- `cl-xmpp` already exposes the simple flow needed for the MVP: connect with TLS, authenticate, send a message.
- Its core ASDF system depends on `usocket`, `fxml`, and `ironclad`; its TLS system adds `cl+ssl`; its SASL system adds `cl-base64` and `cl-sasl`.
- It is old, so vendor or pin it rather than relying on a moving external checkout.

Avoid `cl-ngxmpp` for the first version.

Reasons:

- Its README describes the library as under heavy development.
- Its dependency surface is larger: `blackbird`, `alexandria`, `usocket`, `cxml`, `babel`, `cl+ssl`, `cl-base64`, and `cl-sasl`.
- The CLI only needs one narrow workflow at first: authenticate and send a chat message.

Keep `libstrophe` as a future fallback.

Reasons:

- `libstrophe` is a lightweight C XMPP client library with minimal dependencies and an MIT/GPLv3 dual license.
- It may be better later for modern SASL/TLS compatibility.
- Do not bind the whole library first. If needed, add a tiny C shim that exposes only a simple `send` function to LispWorks FLI/CFFI.

### TLS strategy

Use one of these two approaches, in this order:

1. **MVP / simplest path:** load `cl-xmpp/tls` and accept its `cl+ssl` dependency.
2. **LispWorks-native path:** vendor/fork `cl-xmpp` and patch its TLS upgrade path to use LispWorks `comm:attach-ssl`, which can attach SSL to an existing socket stream and set SNI through `:tlsext-host-name`.

Implement the backend interface so switching between these two approaches does not affect CLI or storage code.

## 3. Repository layout

Create the project with this structure:

```text
xmpp-cli/
  README.md
  plan.md
  xmpp-cli.asd
  scripts/
    build.sh
  build/
    deliver.lisp
  src/
    package.lisp
    util.lisp
    state.lisp
    history.lisp
    xmpp-backend.lisp
    xmpp-cl-xmpp.lisp
    cli.lisp
    main.lisp
  vendor/
    README.md
    cl-xmpp/               # optional vendored/pinned dependency
  test/
    state-tests.lisp
    cli-tests.lisp
```

The MVP can initially omit tests that require a live XMPP server. State and CLI parsing should still be testable without network access.

## 4. ASDF system

Create `xmpp-cli.asd`:

```lisp
(asdf:defsystem "xmpp-cli"
  :description "Small command-line XMPP sender for LispWorks"
  :author "TODO"
  :license "MIT"
  :depends-on ("clingon" "cl-xmpp/tls")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "state")
     (:file "history")
     (:file "xmpp-backend")
     (:file "xmpp-cl-xmpp")
     (:file "cli")
     (:file "main")))))
```

For a vendored LispWorks-native TLS fork, update dependencies later so the project loads the vendored/patched XMPP system rather than upstream `cl-xmpp/tls`.

## 5. Build script and delivery

### `scripts/build.sh`

Create `scripts/build.sh`:

```sh
#!/bin/sh
set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
mkdir -p "$ROOT/build"

exec lw-console -build "$ROOT/build/deliver.lisp"
```

Make it executable:

```sh
chmod +x scripts/build.sh
```

### `build/deliver.lisp`

Create `build/deliver.lisp`:

```lisp
(in-package #:cl-user)

(load-all-patches)

(let* ((script-dir (make-pathname :name nil :type nil :defaults *load-truename*))
       (root       (truename (merge-pathnames "../" script-dir)))
       (ql         (merge-pathnames "quicklisp/setup.lisp"
                                    (user-homedir-pathname))))
  ;; Quicklisp is build-time only. The delivered executable should not need it.
  (when (probe-file ql)
    (load ql))

  #+lispworks
  (require "comm")

  (pushnew root asdf:*central-registry* :test #'equal)

  ;; If vendor/cl-xmpp exists, register it.
  (let ((vendor-cl-xmpp (merge-pathnames "vendor/cl-xmpp/" root)))
    (when (probe-file vendor-cl-xmpp)
      (pushnew vendor-cl-xmpp asdf:*central-registry* :test #'equal)))

  (asdf:load-system "xmpp-cli")

  (ensure-directories-exist (merge-pathnames "build/" root))

  ;; Start at delivery level 0. Raise only after the level-0 binary works.
  (deliver 'xmpp-cli/main:entry-point
           (merge-pathnames "build/xmpp-cli" root)
           0
           :multiprocessing nil))
```

### Delivery notes

- The delivered executable entry point must be `xmpp-cli/main:entry-point`.
- Use `sys:*line-arguments-list*` to read command-line arguments from LispWorks. The first element is the executable name, so pass `(rest sys:*line-arguments-list*)` to the CLI parser.
- Use `lispworks:quit :status N` to return shell exit codes.
- Start delivery at level `0`. Once the binary works, try a higher level and test again.
- Do not require Quicklisp at runtime.

## 6. Packages

Create `src/package.lisp`:

```lisp
(defpackage #:xmpp-cli/util
  (:use #:cl)
  (:export
   #:home-xmpp-cli-directory
   #:ensure-private-directory
   #:write-private-file
   #:read-file-as-string
   #:split-jid
   #:now-iso8601
   #:sha256-hex))

(defpackage #:xmpp-cli/state
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:write-private-file
                #:split-jid)
  (:export
   #:*default-profile-name*
   #:load-config
   #:save-config
   #:profile
   #:set-profile
   #:default-profile-name
   #:config-pathname))

(defpackage #:xmpp-cli/history
  (:use #:cl)
  (:import-from #:xmpp-cli/util
                #:home-xmpp-cli-directory
                #:ensure-private-directory
                #:write-private-file
                #:now-iso8601
                #:sha256-hex)
  (:export
   #:append-history
   #:history-pathname))

(defpackage #:xmpp-cli/backend
  (:use #:cl)
  (:export
   #:send-text
   #:check-login))

(defpackage #:xmpp-cli/backend/cl-xmpp
  (:use #:cl)
  (:import-from #:xmpp-cli/backend
                #:send-text
                #:check-login)
  (:export
   #:make-backend))

(defpackage #:xmpp-cli/cli
  (:use #:cl)
  (:export #:run))

(defpackage #:xmpp-cli/main
  (:use #:cl)
  (:export #:entry-point))
```

Adjust package imports as the actual code evolves.

## 7. Local state format

All local data lives under:

```text
$HOME/.local/xmpp-cli/
```

Create this directory with owner-only permissions where the platform supports it:

```text
mode 0700
```

Files:

```text
$HOME/.local/xmpp-cli/config.sexp
$HOME/.local/xmpp-cli/history.sexp
$HOME/.local/xmpp-cli/logs/
```

`config.sexp` example:

```lisp
(:default-profile "default"
 :profiles
 (("default"
   :jid "user@example.org"
   :username "user"
   :domain "example.org"
   :host "example.org"
   :port 5222
   :resource "xmpp-cli"
   :mechanism :auto
   :password "APP-SPECIFIC-PASSWORD-HERE")))
```

`history.sexp` example:

```lisp
((:time "2026-05-07T12:34:56-07:00"
  :profile "default"
  :to "friend@example.net"
  :kind :text
  :bytes 42
  :sha256 "..."
  :result :sent))
```

Security requirements:

- Do not log passwords.
- Do not log full message bodies by default.
- Store only message metadata and a content hash in history.
- Write config/history files with owner-readable/owner-writable permissions where possible, for example mode `0600`.
- Recommend app-specific XMPP passwords in README.

Implementation hint for permissions:

- On Unix-like systems, LispWorks may expose POSIX/file APIs depending on edition/platform. If there is no portable direct chmod wrapper available, write a small platform-specific helper or call `/bin/chmod` during development. Prefer a LispWorks-native function if available.
- The MVP must still create the correct paths even if permission tightening is best-effort on a particular platform.

## 8. CLI contract

### Top-level

```sh
xmpp-cli --help
xmpp-cli --version
xmpp-cli login ...
xmpp-cli send ...
```

Exit codes:

```text
0  success
1  general runtime error
2  invalid command-line usage
3  missing profile/auth data
4  XMPP connection/authentication/send failure
```

### `login`

MVP command:

```sh
xmpp-cli login <jid> --password-stdin [options]
```

Options:

```text
--profile NAME       Profile name. Default: default
--host HOST          XMPP host. Default: domain part of JID
--port PORT          XMPP client port. Default: 5222
--resource RESOURCE  XMPP resource. Default: xmpp-cli
--mechanism NAME     SASL mechanism keyword. Default: auto
--password-stdin     Read one password line from stdin
```

Examples:

```sh
printf '%s\n' 'secret' | xmpp-cli login user@example.org --password-stdin
printf '%s\n' 'secret' | xmpp-cli login user@example.org --profile work --host xmpp.example.org --password-stdin
```

Behavior:

1. Parse and validate the JID.
2. Read password from stdin when `--password-stdin` is present.
3. Create `$HOME/.local/xmpp-cli/` if missing.
4. Store/update the profile in `config.sexp`.
5. Attempt an actual XMPP connection and authentication using the selected backend.
6. If authentication fails, either:
   - remove the newly stored password/profile, or
   - mark the profile as unverified.

Prefer this safer behavior for the MVP:

- Build the profile in memory.
- Try authentication.
- Only save config after authentication succeeds.

### `send`

MVP commands:

```sh
xmpp-cli send <recipient-jid> "message text"
xmpp-cli send <recipient-jid> -f file
```

Options:

```text
--profile NAME       Profile name. Default: config default profile
-f, --file FILE      Read message body from FILE
```

Validation:

- Exactly one message source is required:
  - positional message text, or
  - `-f/--file`.
- It is an error to provide both positional message text and `-f/--file`.
- It is an error to provide neither.
- File input is treated as UTF-8 text.

Behavior:

1. Load `config.sexp`.
2. Resolve the requested profile.
3. Read the message body.
4. Send message using the selected backend.
5. Append a history entry with metadata, byte count, SHA-256 hash, and result.
6. Print a short success line on stdout, for example:

```text
sent to friend@example.net using profile default
```

## 9. Backend abstraction

Create `src/xmpp-backend.lisp` with a small protocol:

```lisp
(in-package #:xmpp-cli/backend)

(defgeneric check-login (backend profile)
  (:documentation "Return true if PROFILE can authenticate successfully."))

(defgeneric send-text (backend profile to body)
  (:documentation "Send BODY as a chat message to TO using PROFILE."))
```

Create `src/xmpp-cl-xmpp.lisp`:

```lisp
(in-package #:xmpp-cli/backend/cl-xmpp)

(defclass cl-xmpp-backend () ())

(defun make-backend ()
  (make-instance 'cl-xmpp-backend))

(defun required (plist key)
  (or (getf plist key)
      (error "Missing required profile key ~s" key)))

(defmethod check-login ((backend cl-xmpp-backend) profile)
  (declare (ignore backend))
  ;; Implement by connecting/authenticating and then closing the stream.
  ;; Return T on success. Signal a useful condition or error on failure.
  (with-cl-xmpp-connection (connection profile :send-presence nil)
    (declare (ignore connection))
    t))

(defmethod send-text ((backend cl-xmpp-backend) profile to body)
  (declare (ignore backend))
  (with-cl-xmpp-connection (connection profile :send-presence nil)
    (xmpp:message connection to body :type :chat)
    :sent))
```

Implement `with-cl-xmpp-connection` as a macro or helper function that:

1. Reads these profile fields:
   - `:username`
   - `:password`
   - `:domain`
   - `:host`
   - `:port`
   - `:resource`
   - `:mechanism`
2. Calls `xmpp:connect-tls`.
3. Calls `xmpp:auth`.
4. Checks for authentication success.
5. Runs the body.
6. Cleanly closes/disconnects in `unwind-protect`.

Suggested helper shape:

```lisp
(defmacro with-cl-xmpp-connection ((connection profile &key (send-presence nil)) &body body)
  `(call-with-cl-xmpp-connection
    ,profile
    (lambda (,connection) ,@body)
    :send-presence ,send-presence))

(defun call-with-cl-xmpp-connection (profile thunk &key send-presence)
  (let* ((username  (required profile :username))
         (password  (required profile :password))
         (domain    (required profile :domain))
         (host      (or (getf profile :host) domain))
         (port      (or (getf profile :port) 5222))
         (resource  (or (getf profile :resource) "xmpp-cli"))
         (mechanism (or (getf profile :mechanism) :auto))
         (connection nil))
    (unwind-protect
         (progn
           (setf connection
                 (xmpp:connect-tls
                  :hostname host
                  :port port
                  :jid-domain-part domain))
           (let ((result
                   (xmpp:auth connection
                              username
                              password
                              resource
                              :mechanism mechanism
                              :send-presence send-presence)))
             (unless (eq result :authentication-successful)
               (error "XMPP authentication failed: ~s" result)))
           (funcall thunk connection))
      (when connection
        (ignore-errors
          (xmpp:end-xml-stream connection))
        (ignore-errors
          (xmpp:disconnect connection))))))
```

Adjust names if upstream `cl-xmpp` exports differ on LispWorks. Do not let these implementation details leak into CLI code.

## 10. Optional LispWorks-native TLS patch for vendored `cl-xmpp`

If `cl+ssl` delivery is undesirable or fails, patch the vendored `cl-xmpp` TLS conversion to use LispWorks `comm:attach-ssl`.

Conceptual patch:

```lisp
#+lispworks
(progn
  (require "comm")

  (defmethod xmpp::convert-to-tls-stream
      ((connection xmpp:connection)
       &key
         (begin-xml-stream t)
         (receive-stanzas t))
    (comm:attach-ssl
     (xmpp:server-stream connection)
     :ssl-side :client
     :ssl-ctx t
     :handshake-timeout 15
     :tlsext-host-name (xmpp:hostname connection))

    ;; Force cl-xmpp to rebuild its XML source over the now-TLS stream.
    (setf (xmpp::server-source connection) nil)

    (when begin-xml-stream
      (xmpp:begin-xml-stream connection))
    (when receive-stanzas
      (xmpp:receive-stanza connection)
      (xmpp:receive-stanza connection))))
```

Treat this as a patch target, not guaranteed final code. Verify actual slot/accessor names in the vendored `cl-xmpp` source.

## 11. Utility functions

Implement these utilities in `src/util.lisp`.

### Home directory path

```lisp
(defun home-xmpp-cli-directory ()
  (merge-pathnames #P".local/xmpp-cli/" (user-homedir-pathname)))
```

### JID parsing

Implement minimal JID parsing:

```lisp
(defun split-jid (jid)
  "Return USER and DOMAIN from a bare JID user@domain. Signal an error otherwise."
  ...)
```

MVP validation:

- Must contain exactly one `@` for login JID.
- User and domain must be non-empty.
- Recipient JID must be non-empty and should contain `@` for MVP, but do not overvalidate XMPP address grammar yet.

### File reading

```lisp
(defun read-file-as-string (pathname)
  (with-open-file (in pathname
                      :direction :input
                      :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (read-sequence s in)
      s)))
```

If LispWorks requires a different UTF-8 external-format spelling, adjust for LispWorks.

### Hashing

Use `ironclad` if already available through `cl-xmpp` dependencies. Implement `sha256-hex` for history entries.

## 12. Config implementation

Implement `src/state.lisp`.

Data shape:

```lisp
(:default-profile "default"
 :profiles
 (("default" ...plist...)
  ("work" ...plist...)))
```

Functions:

```lisp
(defun config-pathname () ...)
(defun load-config () ...)
(defun save-config (config) ...)
(defun default-profile-name (config) ...)
(defun profile (config name) ...)
(defun set-profile (config name profile-plist &key make-default) ...)
```

Rules:

- If config does not exist, return an empty config:

```lisp
(:default-profile "default" :profiles nil)
```

- Use `with-standard-io-syntax` when reading/writing S-expressions.
- Do not evaluate file contents. Use `read`, not `eval`.
- Fail clearly if the config file is malformed.
- Save atomically where practical:
  - write to `config.sexp.tmp`
  - rename to `config.sexp`

## 13. History implementation

Implement `src/history.lisp`.

Function:

```lisp
(defun append-history (&key profile to kind bytes sha256 result error)
  ...)
```

Rules:

- Append metadata only.
- Preserve existing history.
- If history file is missing, create it.
- If send fails, append a failure entry with `:result :failed` and an error string, but still do not store the message body.

MVP can rewrite the whole history file on each append. Add an append-only format later if necessary.

## 14. CLI implementation with Clingon

Implement `src/cli.lisp`.

Suggested public entry:

```lisp
(defun run (argv)
  "Run xmpp-cli with ARGV excluding executable name. Return an exit code."
  ...)
```

Implement:

- A top-level `xmpp-cli` command.
- A `login` subcommand.
- A `send` subcommand.
- Proper `--help` output.
- Proper usage errors.

Keep command handlers small:

```lisp
(defun handle-login (cmd)
  ...)

(defun handle-send (cmd)
  ...)
```

Handlers should call state/backend/history functions, not implement everything inline.

### Login handler details

Pseudo-flow:

```lisp
(defun handle-login (cmd)
  (let* ((jid      (first positional-args))
         (profile  (option-value cmd "profile"))
         (host     (option-value cmd "host"))
         (port     (option-value cmd "port"))
         (resource (option-value cmd "resource"))
         (mechanism (parse-mechanism ...))
         (password (read-password-from-stdin-if-requested cmd)))
    (multiple-value-bind (username domain) (split-jid jid)
      (let ((profile-plist
              (list :jid jid
                    :username username
                    :domain domain
                    :host (or host domain)
                    :port port
                    :resource resource
                    :mechanism mechanism
                    :password password)))
        (check-login (make-backend) profile-plist)
        (let ((config (load-config)))
          (save-config
           (set-profile config profile profile-plist :make-default t)))
        (format t "logged in as ~a using profile ~a~%" jid profile)
        0))))
```

### Send handler details

Pseudo-flow:

```lisp
(defun handle-send (cmd)
  (let* ((recipient (first positional-args))
         (message-arg ...)
         (file       (option-value cmd "file"))
         (profile-name (or (option-value cmd "profile")
                           (default-profile-name config)))
         (body (if file
                   (read-file-as-string file)
                   message-arg)))
    (handler-case
        (progn
          (send-text (make-backend) profile recipient body)
          (append-history :profile profile-name
                          :to recipient
                          :kind (if file :file-text :text)
                          :bytes (length body)
                          :sha256 (sha256-hex body)
                          :result :sent)
          (format t "sent to ~a using profile ~a~%" recipient profile-name)
          0)
      (error (e)
        (append-history :profile profile-name
                        :to recipient
                        :kind (if file :file-text :text)
                        :bytes (length body)
                        :sha256 (sha256-hex body)
                        :result :failed
                        :error (princ-to-string e))
        (format *error-output* "send failed: ~a~%" e)
        4))))
```

## 15. Main entry point

Implement `src/main.lisp`:

```lisp
(in-package #:xmpp-cli/main)

(defun entry-point ()
  (handler-case
      (let ((argv (rest sys:*line-arguments-list*)))
        (lispworks:quit
         :status (xmpp-cli/cli:run argv)
         :ignore-errors-p t))
    (error (e)
      (format *error-output* "xmpp-cli: ~a~%" e)
      (finish-output *error-output*)
      (lispworks:quit :status 1 :ignore-errors-p t))))
```

For REPL testing, call:

```lisp
(xmpp-cli/cli:run '("--help"))
(xmpp-cli/cli:run '("send" "friend@example.org" "hello"))
```

## 16. Test plan

### Non-network tests

Implement tests or REPL-checkable functions for:

- JID parsing:
  - `user@example.org` -> `user`, `example.org`
  - missing `@` is invalid
  - empty localpart is invalid
  - empty domain is invalid
- Config round-trip:
  - save one profile
  - load it
  - retrieve by profile name
- History append:
  - append success entry
  - append failure entry
  - verify message body is not stored
- CLI validation:
  - `send recipient` fails because no message source
  - `send recipient hello -f file` fails because two message sources
  - `login jid` fails without `--password-stdin`

### Manual network tests

Use a real XMPP account or a local test XMPP server.

Commands:

```sh
printf '%s\n' 'secret' | ./build/xmpp-cli login user@example.org --password-stdin
./build/xmpp-cli send friend@example.org "hello from xmpp-cli"
./build/xmpp-cli send friend@example.org -f ./message.txt
```

Verify:

- `config.sexp` exists under `$HOME/.local/xmpp-cli/`.
- `history.sexp` records sends without message body.
- The recipient receives the text message.
- The binary returns exit code `0` on success.
- The binary returns non-zero on invalid arguments/auth failure/send failure.

### Delivery tests

Run:

```sh
./scripts/build.sh
./build/xmpp-cli --help
./build/xmpp-cli --version
```

Then repeat manual login/send tests with the delivered binary, not only from the LispWorks REPL.

## 17. Phased implementation checklist

### Phase 1: Project skeleton

- [ ] Add `xmpp-cli.asd`.
- [ ] Add package definitions.
- [ ] Add `scripts/build.sh`.
- [ ] Add `build/deliver.lisp`.
- [ ] Add a minimal `entry-point` and `cli:run` that supports `--help`.
- [ ] Confirm `lw-console -build build/deliver.lisp` produces `build/xmpp-cli`.

### Phase 2: Local state

- [ ] Implement `$HOME/.local/xmpp-cli/` path creation.
- [ ] Implement config S-expression read/write.
- [ ] Implement history S-expression read/write.
- [ ] Implement private file/directory permissions best-effort.
- [ ] Add basic state/history tests.

### Phase 3: CLI behavior

- [ ] Implement `login` parser and validation.
- [ ] Implement `send` parser and validation.
- [ ] Implement `--profile`, `--host`, `--port`, `--resource`, `--mechanism`, `--password-stdin`, and `-f/--file`.
- [ ] Implement clear exit codes.
- [ ] Add CLI validation tests.

### Phase 4: Fake backend

- [ ] Implement backend generic functions.
- [ ] Add a fake backend for tests, or a dynamic `*backend*` variable that can be rebound in tests.
- [ ] Confirm `send` can record history without network access.

### Phase 5: `cl-xmpp` backend

- [ ] Add `cl-xmpp/tls` dependency.
- [ ] Implement `check-login`.
- [ ] Implement `send-text`.
- [ ] Test with one real XMPP account.
- [ ] Ensure failures produce useful messages and exit code `4`.

### Phase 6: Delivery hardening

- [ ] Deliver at level `0` and test.
- [ ] Try higher delivery levels only after level `0` succeeds.
- [ ] Ensure the executable does not need Quicklisp at runtime.
- [ ] Inspect whether `cl+ssl` or OpenSSL dynamic libraries are needed at runtime.
- [ ] If external TLS dependencies are unacceptable, start the LispWorks `comm:attach-ssl` patch.

### Phase 7: Documentation

- [ ] Add README with install/build/run examples.
- [ ] Document password handling and recommend app-specific passwords.
- [ ] Document that `send -f` sends text file contents, not an attachment.
- [ ] Document future XEP-0363 upload support separately.

## 18. Future enhancements

- DNS SRV lookup for XMPP client service discovery.
- Interactive password prompt with echo disabled.
- OS credential-store integration where available.
- Multiple default profiles.
- `xmpp-cli profiles list/remove/show`.
- `xmpp-cli history` command.
- XEP-0363 HTTP File Upload for real file attachments.
- Message receipts / delivery markers if supported by the server and recipient client.
- libstrophe backend through a narrow C shim if `cl-xmpp` compatibility is insufficient.

## 19. External references checked while preparing this plan

- Clingon repository: https://github.com/dnaeon/clingon
- Clingon overview/docs: https://dnaeon.github.io/clingon-command-line-options-parse-for-cl/
- `cl-xmpp` repository: https://github.com/atlas-engineer/cl-xmpp
- `cl-xmpp.asd`: https://raw.githubusercontent.com/atlas-engineer/cl-xmpp/master/cl-xmpp.asd
- `cl-ngxmpp` repository: https://github.com/grouzen/cl-ngxmpp
- `cl-ngxmpp.asd`: https://raw.githubusercontent.com/grouzen/cl-ngxmpp/master/cl-ngxmpp.asd
- libstrophe repository: https://github.com/strophe/libstrophe
- libstrophe project page: https://strophe.im/libstrophe/
- LispWorks `comm:attach-ssl`: https://www.lispworks.com/documentation/lw80/lw/lw-comm-34.htm
- LispWorks `sys:*line-arguments-list*`: https://www.lispworks.com/documentation/lw81/lw/lw-os-ug-4.htm
- LispWorks `deliver`: https://www.lispworks.com/documentation/lw81/deliv/deliv-deliver-1.htm
- LispWorks delivery level guidance: https://www.lispworks.com/documentation/lw60/DV/html/delivery-51.htm
- LispWorks runtimes/delivery platform note: https://www.lispworks.com/products/runtimes.html
- XEP-0363 HTTP File Upload: https://xmpp.org/extensions/xep-0363.html
