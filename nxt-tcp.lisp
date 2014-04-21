;;; Client implementation for the proxy server fonud in nxt-proxy.lisp.

(in-package :nxt)

(defclass tcp-nxt (nxt)
  ((connection :initarg :connection :accessor connection)))

(defun open-tcp-nxt (host &optional (port 30000))
  (handler-case
      (usocket:socket-connect host port :element-type '(unsigned-byte 8))
    (:no-error (connection)
      (make-instance 'tcp-nxt :connection connection))
    (usocket:connection-refused-error ()
      nil)
    (usocket:timeout-error ()
      nil)))

(defmethod close-nxt ((nxt tcp-nxt))
  (usocket:socket-close (connection nxt))
  (setf (connection nxt) nil))

(defmethod print-object ((object tcp-nxt) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (connection object))))

;;;; Frame reading: Like the bluetooth protocol, we use an unsigned short
;;;; length value in front of the frame.

(defmethod write-to-nxt ((nxt tcp-nxt) data-vector)
  (let ((stream (usocket:socket-stream (connection nxt))))
    (write-to-nxt-us (length data-vector) stream)
    (write-sequence data-vector stream)
    (force-output stream)))

(defmethod read-from-nxt ((nxt tcp-nxt))
  (let* ((stream (usocket:socket-stream (connection nxt)))
	 (length (read-little-endian-short stream))
	 (result (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence result stream)
    result))
