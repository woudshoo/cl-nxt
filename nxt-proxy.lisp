;;; Contains the bluetooth infrastructure code, communication with the nxt

(in-package :nxt)

(defun mux (tcp nxt)
  (loop
     (unless
	 (let* ((length (read-little-endian-short tcp))
		(data (make-array length :element-type '(unsigned-byte 8))))
	   (unless (plusp length)
	     (return))
	   (read-sequence data tcp)
	   (write-to-nxt nxt data)
	   (logtest #x80 (elt data 0)))
       ;; reply expected
       (let ((data (read-from-nxt nxt)))
	 (write-to-nxt-us (length data) tcp)
	 (write-sequence data tcp)
	 (force-output tcp)))))

(defun handle-connection (nxt connection)
  (print (list nxt connection))
  (handler-case
      (let ((stream (usocket:socket-stream connection)))
	(mux stream nxt))
    (error (c)
      (format t "ignoring error: ~A~%" c))))

(defun beep (nxt freq dur)
  (nxt-play-tone :nxt nxt :frequency freq :duration dur :onewayp t))

(defun run-proxy
  (&optional (port 30000) (host usocket:*wildcard-host*) (beep-on-startup t))
  (let ((nxt (nxt:open-connection '(:usb))))
    (unwind-protect
	(usocket:with-server-socket
	 (listener (usocket:socket-listen
		    host port
		    :reuse-address t
		    :element-type '(unsigned-byte 8)))
	 (when beep-on-startup
	   (beep nxt 500 200))
	 (loop
	  (format t "accepting...~%")
	  (with-simple-restart (abort "abort to nxt proxy")
			       (usocket:with-connected-socket
				(c (usocket:socket-accept listener))
				(when beep-on-startup
				  (beep nxt 800 100))
				(handle-connection nxt c)))))
      (nxt:close-connection))))
