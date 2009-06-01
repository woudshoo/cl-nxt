(in-package :cl-user)

(defpackage :nxt.system
  (:use :cl :asdf))

(in-package :nxt.system)

(defsystem :nxt
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "nxt-bluetooth")
	       (:file "nxt-command-infrastructure")
	       (:file "nxt-commands")
	       (:file "nxt-utilities")))

  