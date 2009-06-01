(in-package #:cl-user)

(defpackage #:nxt
  (:use #:cl)
  (:export #:open-connection
	   #:close-connection))


(pushnew :nxt *features*)
