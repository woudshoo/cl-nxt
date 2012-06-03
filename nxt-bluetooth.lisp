;;; Contains the bluetooth infrastructure code, communication with the nxt

(in-package :nxt)

(defclass bluetooth-nxt (nxt)
  ((connection :initarg :connection :accessor connection)))

(defun open-bluetooth-nxt (device &key (if-does-not-exist :error))
  (let ((connection (open device
			  :direction :io
			  :element-type  '(unsigned-byte 8)
			  :if-does-not-exist if-does-not-exist
			  :if-exists :overwrite)))
    (and connection
	 (make-instance 'bluetooth-nxt :connection connection))))

(defmethod close-nxt ((nxt bluetooth-nxt))
  (close (connection nxt) :abort t)
  (setf (connection nxt) nil))

;;;; Frame reading: The bluetooth protocol wants us to prepend a length
;;;; value to each data vector (unlike the USB protocol!).  Make it so.

(defun read-little-endian-short (stream)
  "Read a 16 bit int from the nxt, lsb first."
  (let ((result 0))
    (setf (ldb (byte 8 0) result) (read-byte stream))
    (setf (ldb (byte 8 8) result) (read-byte stream))
    result))

(defun write-to-nxt-us (short stream)
  "Write a 16 bit int to the nxt lsb first."
  (write-byte (ldb (byte 8 0) short) stream)
  (write-byte (ldb (byte 8 8) short) stream))

(defmethod write-to-nxt ((nxt bluetooth-nxt) data-vector)
  "Writes the raw data vector prefixed by its length.
   This is to wrap the command in the bluetooth 'frame'
   and send it over the wire, eh, air."
  (let ((stream (connection nxt)))
    (write-to-nxt-us (length data-vector) stream)
    (write-sequence data-vector stream)
    (force-output stream)))

(defmethod read-from-nxt ((nxt bluetooth-nxt))
  "Read a reply from the nxt.  It returns the content of the bluetooth
   or USB 'frame', i.e. without length bytes."
  (let* ((stream (connection nxt))
	 (length (read-little-endian-short stream))
	 (result (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence result stream)
    result))
