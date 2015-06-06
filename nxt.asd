;;; Copyright (c) 2010-2011 by Willem Rein Oudshoorn
;;;
;;; Licensed under the BSD License, see accompanied LICENSE file
;;;
(in-package :cl-user)

(defpackage :nxt.system
  (:use :cl :asdf))

(in-package :nxt.system)

(defsystem :nxt
  :depends-on ("cffi" "static-vectors" "babel" #+(and sb-bluetooth sbcl) sb-bluetooth)
  :version "0.0.2"
  :description "Implements the Lego Mindstorm NXT interface over Bluetooth, USB or a TCP proxy."
  :author "David Lichteblau, Willem Rein Oudshoorn <woudshoo@xs4all.nl>"
  :license "BSD, see LICENSE file."
  :serial t
  :components ((:file "package")
	       (:file "nxt-classes")
	       (:file "nxt-bluetooth")
	       (:file "nxt-usb")
	       (:file "nxt-command-infrastructure")
	       (:file "nxt-commands")
	       (:file "nxt-utilities")
	       (:file "nxt-motorcontrol")
	       (:file "nxt-sensors")))
