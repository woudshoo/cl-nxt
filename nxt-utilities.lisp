(in-package :nxt)
   
(defun value-of (key assoc)
  (cdr (assoc key assoc)))

(defun status-ok-p (reply)
  (eq (value-of 'status reply) :success))




(defun find-all-files ()
  (loop 
     :for reply = (nxt-find-first :file-name "*.*") 
     :then (nxt-find-next :handle (value-of 'handle reply))
     :while (status-ok-p reply) 
     :collect  reply
     :finally (nxt-close :handle (value-of 'handle reply))))
     
(defun find-all-modules ()
  (loop 
     :for reply = (nxt-request-first-module :resource-name "*.*") 
     :then (nxt-request-next-module :handle (value-of 'handle reply))
     :while (status-ok-p reply)
     :collect reply
     :finally (nxt-close-module-handle :handle (value-of 'handle reply))))






(defun get-file (file-name)
  (let* ((open-result (nxt-open-read :file-name file-name))
	 (handle (value-of 'handle open-result))
	 (file-size (value-of 'file-size open-result)))
    (if (status-ok-p open-result)
      ;; should break up read in multiple smaller reads
      ;; (I think the protocol does not really like big 
      ;; packages
	(let ((result (make-array file-size)))
	  (loop :for i :below file-size :by 500
	     :for l = (min 500 (- file-size i))
	     :for read-result = (nxt-read :handle handle :nr-of-bytes l)
	     :do
	     (setf (subseq result i) (value-of 'data read-result)))
	  (nxt-close :handle handle)
	  result)
	open-result)))
  
(defun copy-file (file-name-nxt file-name)
  (let ((file-content (get-file file-name-nxt)))
    (with-open-file (s file-name :direction :output 
		       :element-type '(unsigned-byte 8)
		       :if-exists :supersede)
      (write-sequence file-content s))))


(defun put-file (file-name data)
  (let* ((vdata (typecase data
		  (string (map 'vector #'char-code data))
		  (vector data)
		  (t (error "put-file can only handle vectors and strings"))))


	 (open-result (nxt-open-write :file-name file-name :file-size (length vdata))))
    (if (status-ok-p open-result)
	(progn
	  (nxt-write :handle (value-of 'handle open-result) 
		     :data vdata)
	  (nxt-close :handle (value-of 'handle open-result)))
	open-result)))

	 