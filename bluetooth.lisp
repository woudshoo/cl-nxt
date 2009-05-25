;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specification:
;;
;;     (ubyte position)  - byte = 8bits  (== byte)
;;     (uword position)  - uword = 16bits, Little endian
;;     (string position end-position) - string is 0 terminated put in the buffer.
;;     (ulong position)  - ulong = 32bits, Little endian
;;     (data position)   - This should be the last field and the length determines how
;;                         long the command is, or when it is a reply, the
;;                         length of the reply determins the length of the data
;;     (sbyte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS THAT HELP DESTRUCTURING THE SPECIFICATION OF THE COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type-of-spec (spec)
  "Returns the type of a single variable spec for the nxt command description.
Returns one of: 'ubyte, 'uword, 'string."
  (first spec))

(defun last-byte-of-spec (spec)
  "Returns the last byte in the command vector occupied by spec."
  (case (type-of-spec spec)
    (uword (+ 1 (second spec)))
    (ulong (+ 3 (second spec)))
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

  ubyte
  uword
  string  - has additional parameter, `final-position'.

The result is something that looks like

  (create-nxt-command 'find-first           #x01 #x86 'file-name '(string 2 21))

==>
  (defun find-first (&key file-name)
      (let ((data-vector (make-array 22 :element-type 'unsigned-byte :initial-element 0)))
          (write-ubyte-to-command data-vector #x01)
          (write-ubyte-to-command data-vector #x86)
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
	(write-ubyte-to-command data-vector ,type-code 0)
	(write-ubyte-to-command data-vector ,command-code 1)
	,(cons 'progn (loop :for (key spec) :on args :by #'cddr :collect
	    (case (type-of-spec spec)
	      (ubyte `(write-ubyte-to-command data-vector ,key ,(second spec)))
	      (string `(write-string-to-command data-vector ,key ,(second spec) ,(third spec)))
	      (uword `(write-uword-to-command data-vector ,key ,(second spec)))
	      (ulong `(write-ulong-to-command data-vector ,key ,(second spec)))
	      (t (error "Unknow type ~S in specification" (type-of-spec spec))))))
	(write-to-nxt data-vector)
	,(when (reply-expected-for-type-code type-code)
	       '(let ((reply (read-nxt-reply)))
		 (parse-nxt-reply (aref reply 1) reply)))))))

(defmacro def-nxt-command (name name-code type-code &rest rest)
  (apply #'create-nxt-command-form name name-code type-code rest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATING THE COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-nxt-command get-firmware-version #x01 #x88)
(def-nxt-command get-device-info      #x01 #x9b)
(def-nxt-command find-first           #x01 #x86 file-name (string 2 21))
(def-nxt-command find-next            #x01 #x87 handle    (ubyte 2))
(def-nxt-command nxt-close                #x01 #x84 handle     (ubyte 2))


;; (def-nxt-command get-input-values     #x00 #x07 port       (ubyte 2))
;; (def-nxt-command get-output-state     #x00 #x06 port       (ubyte 2))
;; (def-nxt-command get-battery-level    #x00 #x0b)
;; (def-nxt-command stop-sound-playback  #x00 #x0c)
;; (def-nxt-command set-input-mode       #x00 #x05 port       (ubyte 2) 
;; 		 sensor-type          (ubyte 3)  sensor-mode (ubyte 4))
;; (def-nxt-command message-read         #x00 #x13 
;; 		 remote-inbox         (ubyte 2)  local-inbox (ubyte 3) remove (ubyte 4))

;; (def-nxt-command get-device-info      #x01 #x9b)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPER FUNCTIONS TO BUILD THE COMMAND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-ubyte-to-command (vector value location)
  (assert (arrayp vector) (vector) "First argument should be a vector!")
  (setf (aref vector location) value))

(defun read-ubyte-from-reply (vector location)
  (assert (arrayp vector) (vector) "First argument should be a vector!")
  (aref vector location))


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

(defun read-string-from-reply (vector start end)
  "Reads the zero terminates tring from vector."
  (with-output-to-string (s)
    (loop :for index :from start :below end
       :for char = (aref vector index)
       :until (eql char 0)
       :do
       (write-char (code-char char) s))))

(defun write-uword-to-command (vector value location)
  (setf (aref vector location) (ldb (byte 8 0) value))
  (setf (aref vector (1+ location)) (ldb (byte 8 8) value)))

(defun write-ulong-to-command (vector value location)
  (setf (aref vector location) (ldb (byte 8 0) value))
  (setf (aref vector (+ 1 location)) (ldb (byte 8 8) value))
  (setf (aref vector (+ 2 location)) (ldb (byte 8 16) value))
  (setf (aref vector (+ 3 location)) (ldb (byte 8 24) value)))
 
(defun read-uword-from-reply (vector location)
  (let ((result 0))
    (setf (ldb (byte 8 0) result) (aref vector location))
    (setf (ldb (byte 8 8) result) (aref vector (1+ location)))
    result))
      
(defun read-ulong-from-reply (vector location)
  (let ((result 0))
    (setf (ldb (byte 8 0) result) (aref vector location))
    (setf (ldb (byte 8 8) result) (aref vector (1+ location)))
    (setf (ldb (byte 8 16) result) (aref vector (+ 2 location)))
    (setf (ldb (byte 8 24) result) (aref vector (+ 3 location)))
    result))
    
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSING OUTPUT ???
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; want something to specify the pattern of the output command
;;
;; (def-reply-package #x81 handle (byte 3) size (ulong 4))
;; (def-reply-package #x 
;; The expansion will give a generic method such as:
;;
;(defmethod parse-nxt-reply ((code (eql #x81)) (data vector))
;  (list (cons 'handle (

(defmacro def-reply-package (code &rest rest)
  (list
   'defmethod 'parse-nxt-reply 
   (list (list 'code (list 'eql code))
	 (list 'data 'vector))
   (cons 'list
    (loop :for  (key spec) :on (append (list 'status '(ubyte 2))  rest) :by #'cddr :collect
       (list 'cons `(quote ,key)
	     (case (type-of-spec spec)
	       (ubyte `(read-ubyte-from-reply data ,(second spec)))
	       (string `(read-string-from-reply data ,(second spec) ,(third spec)))
	       (uword `(read-uword-from-reply data ,(second spec)))
	       (ulong `(read-ulong-from-reply data ,(second spec)))
	       (t (error "Unrecognized type ~S in specification" (type-of-spec spec)))))))))


;; open read command
(def-reply-package #x80 handle (ubyte 3)
		        file-size (ulong 4))
;; oen write command
(def-reply-package #x81 handle (ubyte 3))
;; read command
;(def-reply-package #x82 handle (byte 3)
;		        amount-read (uword 4)
;			data (data 6))
;; write command
(def-reply-package #x83 handle (ubyte 3)
		   amount-written-to-flash (uword 4))
;; close command
(def-reply-package #x84 handle (ubyte 3))
;; delete command
(def-reply-package #x85 file-name (string 3 22))
;; find-first
(def-reply-package #x86 handle (ubyte 3)
		        file-name (string 4 23)
			file-size (ulong 24))
;; find-next
(def-reply-package #x87 handle (ubyte 3)
		        file-name (string 4 23)
			file-size (ulong 24))
;; Get firmware version
(def-reply-package #x88 protocol-version-minor (ubyte 3)
		        protocol-version-major (ubyte 4)
                        firmware-version-minor (ubyte 5)
                        firmware-version-major (ubyte 6))

;; Get device info
(def-reply-package #x9b 
    nxt-name (string 3 17)
    bluetooth-signal-strength (ulong 25)
    free-flash (ulong 29))



;;;; 
;; MODULES
;;;
(def-nxt-command request-first-module    #x01 #x90 resource-name (string 2 21))
(def-reply-package #x90 handle (ubyte 3) module-name (string 4 23)
		   module-id (ulong 24)
		   module-size (ulong 28)
		   io-map-size (uword 32))

(def-nxt-command request-next-module #x01 #x91 handle (ubyte 2))
(def-reply-package #x91 handle (ubyte 3) module-name (string 4 23)
		   module-id (ulong 24)
		   module-size (ulong 28)
		   io-map-size (uword 32))

(def-nxt-command close-module-handle #x01 #x92 handle (ubyte 2))
(def-reply-package #x92 handle (ubyte 3))

;;;;
;; DIRECT COMMANDS
;;;;;
(def-nxt-command start-program #x00 #x00 file-name (string 2 21))
(def-reply-package #x00)

(def-nxt-command stop-program  #x00 #x01)
(def-reply-package #x01)

(def-nxt-command play-sound-file #x00 #x02 loop (ubyte 2) file-name (string 3 22))
(def-reply-package #x02)

(def-nxt-command play-tone #x00 #x03 frequency (uword 2) duration (uword 4))
(def-reply-package #x03)

(def-nxt-command set-output-state #x00 #x04 
		 output-port (ubyte 2) 
		 power-set-point (ubyte 3)
		 mode (ubyte 4)
		 regulation-mode (ubyte 5)
		 turn-ratio (ubyte 6) ;; needs to be subyte, so this will fail with negative numebrs.
		 run-state (ubyte 7)
		 tacho-limit (ulong 8))

(def-reply-package #x04)		 

(def-nxt-command set-input-mode #x00 #x05
		 input-port (ubyte 2)
		 sensor-type (ubyte 3)
		 sensor-mode (ubyte 4))
(def-reply-package #x05)		 

(def-nxt-command get-output-state #x00 #x06
		 output-port (ubyte 2))
(def-reply-package #x06
    output-port (ubyte 3)
    power-set-point (ubyte 4)
    mode (ubyte 5)
    regulation-mode (ubyte 6)
    turn-ratio (ubyte 7) ;; needs to be sbyte
    run-state (ubyte 8)
    tacho-limit (ulong 9)
    tacho-count (ulong 13) ;; should be slong
    block-tacho-count (ulong 17) ;; should be slong)
    rotation-count (ulong 21) ;; should be slong
)

(def-nxt-command get-input-values #x00 #x07 input-port (ubyte 2))
(def-reply-package #x07
    input-port (ubyte 3)
    valid (ubyte 4)
    calibrated (ubyte 5)
    sensor-type (ubyte 6)
    sensor-mode (ubyte 7)
    raw-a/d-value (uword 8)
    normalized-a/d-value (uword 10)
    scaled-value (uword 12)  ;; should be sword
    calibrated-value (uword 14) ;; should be sword
)

(def-nxt-command reset-input-scaled-value #x00 #x08 input-port (ubyte 2))
(def-reply-package #x08)

;(def-nxt-command message-write #x00 #x09 inbox (ubyte 2) message-size (ubyte 3) message-data (data 4)) ;; Note need to think about data
;(def-reply-package #x09)

(def-nxt-command reset-motor-position #x00 #x0a output-port (ubyte 2) relative (ubyte 3))
(def-reply-package #x0a)

(def-nxt-command get-battery-level #x00 #x0b)
(def-reply-package #x0b voltage (uword 3))

(def-nxt-command stop-sound-playback #x00 #x0c)
(def-reply-package #x0c)

(def-nxt-command keep-alive #x00 #x0d)
(def-reply-package #x0d sleep-time-limit (ulong 3))


(def-nxt-command ls-get-status #x00 #x0e port (ubyte 2))
(def-reply-package #x0e bytes-ready (ubyte 3))

;; (def-nxt-command ls-write #x00 #x0f port (ubyte 2)
;; 		 tx-data-length (ubyte 3)
;; 		 rx-data-length (ubyte 4)
;; 		 tx-data (data 5))
;(def-reply-package #x0f)

(def-nxt-command ls-read #x00 #x10 port (ubyte 2))
;(def-reply-package #x10 bytes-read (byte 3) rx-data (data 3 19))
		 

(def-nxt-command get-current-program-name #x00 #x11)
(def-reply-package #x11 file-name (string 3 22))

(def-nxt-command message-read #x00 #x13
		 remote-inbox (ubyte 2)
		 local-inbox (ubyte 3)
		 remove (ubyte 4))
;; (def-reply-package #x13 local-inbox (byte 3) message-size (byte 4) message-data (data 5 63)) 

    

		 
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod parse-nxt-reply ((code t) (data vector))
"Default fallback method."
  data)
   
(defun value-of (key assoc)
  (cdr (assoc key assoc)))

(defun dump-message (message)
  (loop :for x :across message 
     :for i :from 0  :do
     (format t "~2D: ~3D ~16R~%" i x x)))

(defun status (reply)
  (aref reply 2))

(defun handle (reply)
  (aref reply 3))



;;;;;;;;; Higher level functions

(defun find-all-files ()
  (loop 
     :for reply = (find-first :file-name "*.*") 
     :then (find-next :handle (value-of 'handle reply))
     :while (= (value-of 'status reply) 0)
     :collect (value-of 'file-name reply)
     :finally (nxt-close :handle (value-of 'handle reply))))
     
(defun find-all-modules ()
  (loop 
     :for reply = (request-first-module :resource-name "*.*") 
     :then (request-next-module :handle (value-of 'handle reply))
     :while (= (value-of 'status reply) 0)
     :collect reply
     :finally (close-module-handle :handle (value-of 'handle reply))))
     