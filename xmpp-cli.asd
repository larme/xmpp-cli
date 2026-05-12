;;;; -*- mode: lisp -*-

(in-package #:cl-user)

(asdf:defsystem "xmpp-cli"
  :description "Small command-line XMPP sender for LispWorks"
  :author "TODO"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("clingon"
               "cl-base64"
               "cl-xmpp/tls"
               "com.inuoe.jzon"
               "bordeaux-threads"
               "flexi-streams"
               "ironclad"
               "usocket"
               "uiop")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "yaml")
     (:file "persistence")
     (:file "file-lock")
     (:file "json")
     (:file "state")
     (:file "history")
     (:file "agent-config")
     (:file "agent-routes")
     (:file "tmux")
     (:file "agent-codex")
     (:file "xmpp-backend")
     (:file "agent-ipc")
     (:file "agent-daemon-state")
     (:file "agent-replies")
     (:file "agent-control")
     (:file "agent-daemon")
     (:file "xmpp-cl-xmpp")
     (:file "sender")
     (:file "cli")
     (:file "main")))))

(asdf:defsystem "xmpp-cli/test"
  :description "Non-network tests for xmpp-cli"
  :depends-on ("xmpp-cli" "uiop")
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "test-support")
     (:file "state-tests")
     (:file "scram-tests")
     (:file "cli-tests")
     (:file "agent-tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :xmpp-cli/test :run-tests)))
