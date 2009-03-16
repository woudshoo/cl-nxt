(defvar *command-table* nil) 

(defun fill-command-table ()
  (def-nxt-command 'get-firmware-version #x01 #x88)
  (def-nxt-command 'get-device-info      #x01 #x9b)
  (def-nxt-command 'find-first           #x01 #x86 :file-name '(string 2 21))
  (def-nxt-command 'find-next            #x01 #x87 :handle '(byte 2))
  (def-nxt-command 'close                #x01 #x84 :handle '(byte 2))
  (def-nxt-command 'play-tone            #x00 #x03 :frequency '(uword 2) :duration '(uword 4))
  (def-nxt-command 'get-input-values     #x00 #x07 :port '(byte 2))
  (def-nxt-command 'get-output-state     #x00 #x06 :port '(byte 2))
  (def-nxt-command 'get-battery-level    #x00 #x0b)
  (def-nxt-command 'stop-sound-playback  #x00 #x0c)
  (def-nxt-command 'set-input-mode       #x00 #x05 :port '(byte 2) 
		   :sensor-type '(byte 3) :sensor-mode '(byte 4))
  (def-nxt-command 'message-read         #x00 #x13 
		   :remote-inbox '(byte 2) :local-inbox '(byte 3) :remove '(byte 4)))


(defun command-data-for-id (command-id)
  (let ((result (cdr (assoc command-id *command-table*))))
    (unless result (error "No command found for id: ~S~%" command-id))
    result))

(defun def-nxt-command (command-id command-type code &rest rest)
  (setf *command-table* (remove command-id *command-table* :key #'car))
  (setf *command-table* (push
			 (cons  command-id 
				(loop :for (key value . ignored) :on rest :by #'cddr
				   :maximize 
				   (case (car value)
				     ('uword  (1+ (second value)))
				     (t (apply #'max (cdr value)))) :into max-length
				   :finally (return 
					      (cons (+ command-type (* 256 code))
						    (cons (max 2 (1+ max-length)) rest)))))
			 *command-table*)))

(defun write-byte-to-command (vector value location)
  (assert (arrayp vector) (vector) "First argument should be a vector!")
  (setf (aref vector location) value))

(defun write-string-to-command (vector value start end)
  "Write the string `value' in the result at the byte range
`start' .. `end'.   We assume the range is closed, that is, 
both start and end are part of the string space in the vector.
However we also assume that the string is \0 terminated and
that the space we write in is already zeroed out."
  (loop :for char :across value 
     :for index :from start :below end
     :do
     (setf (aref vector index) (char-code char))))

(defun write-uword-to-command (vector value location)
  (setf (aref vector location) (ldb (byte 8 0) value))
  (setf (aref vector (1+ location)) (ldb (byte 8 8) value)))

(defun make-command (command-id &rest args)
  "Creates a nxt command of type indicated by `command-id'.
All the arguments are keyword value pairs.  These should
be matched in the command struct for the command.

So for example:
 
  (make-command 'find-first :file-name \"*.*\")

creates a valid command."
  (let* ((command-data (command-data-for-id command-id))
	 (command-length (second command-data))
	 (result (make-array command-length 
			     :element-type 'unsigned-byte
			     :initial-element 0)))
    ;; First write first few bytes.
    (write-uword-to-command result (first command-data) 0)

    ;; Now fill in the rest.
    (loop :for (key value . rest) :on args :by #'cddr 
       :for key-datum = (getf command-data key) :do
       (progn
	 (unless key-datum (error "Can not find key: ~S back in data for command." key))
	 (case (first key-datum)
	   ('byte (write-byte-to-command result value (second key-datum)))
	   ('string (write-string-to-command result value (second key-datum) (third key-datum)))
	   ('uword (write-uword-to-command result value (second key-datum))))))
    result))

(defvar *connection* nil)

(defun open-connection (&optional (dev-name "/dev/tty.NXT-DevB-1"))
  (when *connection*
    (error "Default conection is already open, close it first!"))
  ; hope default is the correct, according to the standard a nice implemention
  ; will indeed pick the best element-type for the file.
  (setf *connection* (open dev-name :direction :io :element-type :default :if-exists :overwrite))
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
  (read-byte *connection*))
			   
(defun write-to-nxt-us (short)
  "Write a 16 bit int to the nxt lsb first."
  (write-to-nxt-byte (ldb (byte 8 0) short))
  (write-to-nxt-byte (ldb (byte 8 8) short)))

(defun read-from-nxt-us ()
  (let ((result 0))
    (setf (ldb (byte 8 0) result) (read-from-nxt-byte))
    (setf (ldb (byte 8 8) result) (read-from-nxt-byte))
    result))

(defun write-to-nxt (data-vector)
  "Writes the raw data vector prefixed by its length.
This is to wrap the command in the blue tooth 'frame' 
and send it over the wire, eh, air."
  (write-to-nxt-us (length data-vector))
  (loop :for byte :across data-vector :do
     (write-to-nxt-byte byte))
  (force-output *connection*))

(defun reply-expected (command)
  "Returns T if a reply is expected.
A reply is expected if the bit in position 7 (MSB) is 0"
  (eql 0 (ldb (byte 1 7) (aref command 0))))

    
(defun read-nxt-reply ()
  (let* ((length (read-from-nxt-us))
	 (result (make-array length :element-type 'unsigned-byte :initial-element 0)))
    (loop :for i :from 0 :below length :do
       (setf (aref result i) (read-from-nxt-byte)))
    result))

(defun send-to-nxt (command)
  "Write command to nxt and read reply if a reply is expected."
  (write-to-nxt command)
  (when (reply-expected command)
    (read-nxt-reply)))


(defun dump-message (message)
  (loop :for x :across message 
     :for i :from 0  :do
     (format t "~2D: ~3D ~16R~%" i x x)))

(defun test (&optional (command 'get-firmware-version))
  (let ((reply (send-to-nxt (make-command command))))
    (format t "~A~%" reply)
    reply))
     

(defun status (reply)
  (aref reply 2))

(defun handle (reply)
  (aref reply 3))