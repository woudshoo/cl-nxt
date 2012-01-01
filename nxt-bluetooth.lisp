;;; Contains the bluetooth infrastructure code, communication with the nxt

(in-package :nxt)

(defparameter *nxt-bluetooth-device* "/dev/tty.NXT-DevB-1")
(defvar *connection* nil)

(defun open-connection (&optional (dev-name *nxt-bluetooth-device*))
  "Opens a bluetooth serial connection on device given by `dev-name'.
For the rest of the code to work, the device opened should speak the NXT 
bluetooth protocol.  This is normally achieved by tying the `dev-name' device
to a NXT brick.  

After the connectionis opened, it is stored in the special variable `*connection*'
which is used for all the commands that actually talk to the NXT brick."
  (when *connection*
    (error "Default conection is already open, close it first!"))
  (setf *connection* (open dev-name :direction :io :element-type  '(unsigned-byte 8) :if-exists :overwrite))
  (unless *connection*
    (error "Failed to open bluetooth device!")))

(defun close-connection ()
  (unless *connection*
    (error "Connection is already closed."))
  (close *connection*)
  (setf *connection* nil))


(defun write-to-nxt-byte (byte)
  "Write a raw byte to the nxt."
  (write-byte byte *connection*))

(defun read-from-nxt-byte ()
  "Read a byte from te nxt."
  (read-byte *connection*))
			   
(defun write-to-nxt-us (short)
  "Write a 16 bit int to the nxt lsb first."
  (write-to-nxt-byte (ldb (byte 8 0) short))
  (write-to-nxt-byte (ldb (byte 8 8) short)))

(defun read-from-nxt-us ()
  "Read a 16 bit int from the nxt, lsb first."
  (let ((result 0))
    (setf (ldb (byte 8 0) result) (read-from-nxt-byte))
    (setf (ldb (byte 8 8) result) (read-from-nxt-byte))
    result))

(defun write-to-nxt (data-vector)
  "Writes the raw data vector prefixed by its length.
This is to wrap the command in the bluetooth 'frame' 
and send it over the wire, eh, air."
  (write-to-nxt-us (length data-vector))
  (loop :for byte :across data-vector :do
     (write-to-nxt-byte byte))
  (force-output *connection*))
    
(defun read-nxt-reply ()
  "Read a reply from the nxt.  It returns the content
of the bluetooth 'frame', NOT the frame itself."
  (let* ((length (read-from-nxt-us))
	 (result (make-array length :element-type 'unsigned-byte :initial-element 0)))
    (loop :for i :from 0 :below length :do
       (setf (aref result i) (read-from-nxt-byte)))
    result))

