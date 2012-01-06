;;; Copyright (c) 2010-2011 by Willem Rein Oudshoorn
;;;
;;; Licensed under the BSD License, see accompanied LICENSE file
;;;
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

  