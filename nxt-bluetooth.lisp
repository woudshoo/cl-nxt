;;; Contains the bluetooth infrastructure code, communication with the nxt

(in-package :nxt)

(defclass bluetooth-nxt (nxt)
  ((connection :initarg :connection :accessor connection)
   (read-timestamp :initform 0 :accessor read-timestamp)
   (write-timestamp :initform 0 :accessor write-timestamp)))

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
;;;;
;;;; We are also required by the protocol to observe certain timeouts --
;;;; otherwise the brick will not respond meaningfully.

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
  (wait-for-bluetooth nxt :write)
  (let ((stream (connection nxt)))
    (write-to-nxt-us (length data-vector) stream)
    (write-sequence data-vector stream)
    (force-output stream))
  (note-data-written nxt))

(defmethod read-from-nxt ((nxt bluetooth-nxt))
  "Read a reply from the nxt.  It returns the content of the bluetooth
   or USB 'frame', i.e. without length bytes."
  (wait-for-bluetooth nxt :read)
  (let* ((stream (connection nxt))
	 (length (read-little-endian-short stream))
	 (result (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence result stream)
    (note-data-read nxt)
    result))

(defun wait-for-bluetooth (nxt delay-type)
  (let ((timeout (compute-bluetooth-timeout nxt delay-type)))
    (when (plusp timeout)
      (sleep timeout))))

;;; I'm not entirely certain what the spec for the timeout issues is.
;;; But if I don't have delays in the code at all, the brick regularly fails
;;; to respond.  And other NXT libraries use a strategy like the following,
;;; so it ought to work for us too.

(defparameter *bluetooth-delay-between-writes* 0.005
  "Required delay in seconds between successive writes to the brick.")

(defparameter *bluetooth-delay-between-directions* 0.025
  "Required delay in seconds when switching between reading from
   and writing to the brick in either direction.")

(defun %compute-bluetooth-timeout (timestamp delay)
  (let* ((now (get-internal-real-time))
	 (elapsed (/ (- now timestamp) internal-time-units-per-second)))
    (- delay elapsed)))

(defgeneric compute-bluetooth-timeout (nxt delay-type)
  (:method (nxt (delay-type (eql :read)))
    (max 0
	 (%compute-bluetooth-timeout (write-timestamp nxt)
				     *bluetooth-delay-between-directions*)))
  (:method (nxt (delay-type (eql :write)))
    (max 0
	 (%compute-bluetooth-timeout (write-timestamp nxt)
				     *bluetooth-delay-between-writes*)
	 (%compute-bluetooth-timeout (read-timestamp nxt)
				     *bluetooth-delay-between-directions*))))

(defun note-data-read (nxt)
  (setf (read-timestamp nxt) (get-internal-real-time)))

(defun note-data-written (nxt)
  (setf (write-timestamp nxt) (get-internal-real-time)))
