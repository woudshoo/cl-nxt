(defsystem :nxt-proxy
  :depends-on ("nxt" "usocket")
  :serial t
  :components ((:file "nxt-proxy") (:file "nxt-tcp")))
