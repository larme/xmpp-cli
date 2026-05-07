# Vendor Dependencies

`vendor/cl-xmpp/` contains the pinned XMPP backend used by the MVP. The build script registers this directory with ASDF before quickloading `xmpp-cli`, so the local `cl-xmpp/tls` system is preferred over any moving external checkout.

If `cl+ssl` delivery becomes a problem on the target LispWorks platform, patch this vendored copy to use LispWorks `comm:attach-ssl` behind the existing backend abstraction.
