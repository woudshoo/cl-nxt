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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS THAT HELP DESTRUCTURING THE SPECIFICATION OF THE COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type-of-spec (spec)
  "Returns the type of a single variable spec for the nxt command description.
Returns one of: 'byte, 'uword, 'string."
  (first spec))

(defun last-byte-of-spec (spec)
  "Returns the last byte in the command vector occupied by spec."
  (case (type-of-spec spec)
    ('uword (+ 1 (second spec)))
    (t (car (last spec)))))
    
(defun command-length (command-spec)
  "Returns the length of the command buffer needed to hold
all the variables specified in `command-spec'."
  (max 2
   (1+ (loop :for (name spec) :on command-spec :by #'cddr
     :maximize (last-byte-of-spec spec)))))

(defun reply-expected-for-type-code (code)
    (eql 0 (ldb (byte 1 7) code)))

;; TRANSLATING THE FORMS INTO COMMANDS
(defun create-nxt-command-form (name type-code command-code &rest args)
  "Creates the command function `name' which is a nxt command with the
two command identification bytes `type-code' and `command-code'.  
The `args' contains key value pairs that should be taken by the function together
with a type specifier.  Type specifiers are lists of the form (`type' `position' rest).
The type is one of 

  byte
  uword
  string  - has additional parameter, `final-position'.

The result is something that looks like

  (create-nxt-command 'find-first           #x01 #x86 'file-name '(string 2 21))

==>
  (defun find-first (&key file-name)
      (let ((data-vector (make-array 22 :element-type 'unsigned-byte :initial-element 0)))
          (write-byte-to-command data-vector #x01)
          (write-byte-to-command data-vector #x86)
          (write-string-to-command data-vector file-name 2 21)
          (write-to-nxt data-vector)
          (read-nxt-reply)))


"
  (let ((command-length (command-length args)))
    (list
     'defun name (cons '&key (loop :for key :in args :by #'cddr :collect key ))
     `(let ((data-vector (make-array ,command-length 
				     :element-type 'unsigned-byte
				     :initial-element 0)))
	(write-byte-to-command data-vector ,type-code 0)
	(write-byte-to-command data-vector ,command-code 1)
	,(cons 'progn (loop :for (key spec) :on args :by #'cddr :collect
	    (case (type-of-spec spec)
	      (byte `(write-byte-to-command data-vector ,key ,(second spec)))
	      (string `(write-string-to-command data-vector ,key ,(second spec) ,(third spec)))
	      (uword `(write-uword-to-command data-vector ,key ,(second spec)))
	      (t "Help"))))
	(write-to-nxt data-vector)
	,(when (reply-expected-for-type-code type-code)
	       '(read-nxt-reply))))))

(defmacro def-nxt-command (name name-code type-code &rest rest)
  (apply #'create-nxt-command-form name name-code type-code rest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATING THE COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-nxt-command get-firmware-version #x01 #x88)
(def-nxt-command get-device-info      #x01 #x9b)
(def-nxt-command find-first           #x01 #x86 file-name (string 2 21))
(def-nxt-command find-next            #x01 #x87 handle    (byte 2))
(def-nxt-command close                #x01 #x84 handle     (byte 2))
(def-nxt-command play-tone            #x00 #x03 frequency  (uword 2)       duration (uword 4))
(def-nxt-command get-input-values     #x00 #x07 port       (byte 2))
(def-nxt-command get-output-state     #x00 #x06 port       (byte 2))
(def-nxt-command get-battery-level    #x00 #x0b)
(def-nxt-command stop-sound-playback  #x00 #x0c)
(def-nxt-command set-input-mode       #x00 #x05 port       (byte 2) 
		 sensor-type          (byte 3)  sensor-mode (byte 4))
(def-nxt-command message-read         #x00 #x13 
		 remote-inbox         (byte 2)  local-inbox (byte 3) remove (byte 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPER FUNCTIONS TO BUILD THE COMMAND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONNECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOWLEVEL COMMUNICATION TO THE NXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    
(defun read-nxt-reply ()
  (let* ((length (read-from-nxt-us))
	 (result (make-array length :element-type 'unsigned-byte :initial-element 0)))
    (loop :for i :from 0 :below length :do
       (setf (aref result i) (read-from-nxt-byte)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEFT OVER FROM OLD CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun send-to-nxt (command)
  "Write command to nxt and read reply if a reply is expected."
  (write-to-nxt command)
  (when (reply-expected command)
    (read-nxt-reply)))

(defun reply-expected (command)
  "Returns T if a reply is expected.
A reply is expected if the bit in position 7 (MSB) is 0"
  (eql 0 (ldb (byte 1 7) (aref command 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSING OUTPUT ???
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump-message (message)
  (loop :for x :across message 
     :for i :from 0  :do
     (format t "~2D: ~3D ~16R~%" i x x)))

(defun status (reply)
  (aref reply 2))

(defun handle (reply)
  (aref reply 3))
