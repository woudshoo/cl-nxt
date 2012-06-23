;;; Copyright (c) 2010-2011 by Willem Rein Oudshoorn
;;;
;;; Licensed under the BSD License, see accompanied LICENSE file
;;;

(in-package :nxt)

;;;;;
;;;;; nxt-command-infrastructure
;;
;;
;; This file consists of two parts,
;;
;; 1 - infrastructure to build command vectors
;; 2 - macros to define commands + supporting functions to create the macro
;;


;;;; Part 1
;; Infrastructure to build command vectors
;;
;; A command vector is a vector containing bytes/octets that form
;; a nxt command.  It will not include the 'length' byte which is part of
;; the bluetooth wrapping layer.
;;
;; Note that both the command vector and reply vector are of course
;; octets over the wire, but are on the lisp side seen as vector
;; of unsigned bytes.
;;
(defun write-ubyte-to-command (vector value location)
  "Put the value `value' in the command vector `vector' at location `location'."
  (setf (aref vector location) value))

(defun read-ubyte-from-reply (vector location)
  "Extract unsigned byte value from the reply vector `vector' at location `location'."
  (aref vector location))

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


;;;; Part 2
;; Macros to defin nxt-commands and their supporting functions.


;;;;
;;;; FRAME-INFO: Description of the two frames (request and reply) which
;;;; together define a command. 
;;;;

(defclass frame-pair ()
  ((name         :initarg :name         :accessor fp-name)
   (type-code    :initarg :type-code    :accessor fp-type-code)
   (command-code :initarg :command-code :accessor fp-command-code)
   (request      :initarg :request      :accessor fp-request)
   (reply        :initarg :reply        :accessor fp-reply)))

(defun find-frame-pair (name &optional (errorp t))
  (or (get name 'frame-pair)
      (when errorp
	(error "no frame pair found of name ~A" name))))

(defvar *code-to-info-table* (make-array 256 :initial-element nil))

(defun (setf find-frame-pair) (newval name)
  (setf (get name 'frame-pair) newval)
  (setf (elt *code-to-info-table* (fp-command-code newval)) newval))

(defun clear-all ()
  ;; clear out everything manually, just in case the two maps are out of sync
  (do-symbols (sym (find-package :nxt))
    (when (find-frame-pair sym nil)
      (setf (get sym 'frame-pair) nil)))
  (fill *code-to-info-table* nil))

(defun ensure-frame-pair (name type-code command-code)
  (or (find-frame-pair name nil)
      (setf (find-frame-pair name)
	    (make-instance 'frame-pair
			   :name name
			   :type-code type-code
			   :command-code command-code))))

(defun lookup-command-code (command-code)
  (elt *code-to-info-table* command-code))


;;;;
;;;; FRAME-INFO: Description of a single frame's (either direction)
;;;; parameters.
;;;;

(defclass frame-info ()
  ((n-static-bytes :initform nil        :accessor %n-static-bytes)
   (parameters     :initarg :parameters :accessor fi-parameters)
   (explicit-parameters                 :accessor fi-explicit-parameters)))

(defun parse-frame-info (specs)
  (let ((parameters (parse-parameter-specs specs)))
    (make-instance 'frame-info :parameters parameters)))

;;; Traditionally, the file nxt-commands.lisp listed byte offsets for all
;;; parameters manually.  We can actually compute those offsets automatically
;;; based on the fields and their sizes.  But since the LEGO spec lists
;;; all offsets, there is not much point to doing it automatically.
;;; Let's explicitly have an assertion though, to ensure that the sizes
;;; match up with the specified offsets though:
;;;
(defun check-offsets (parameters)
  (when parameters
    (assert (eql 2 (pi-offset (car parameters)))))
  (loop for (a b) on parameters do
       (when b
	 (unless (eql (+ (pi-offset a) (pi-nbytes a))
		      (pi-offset b))
	   (error "parameter positions invalid: ~A/~A" a b)))))

(defun link-size-parameters! (params)
  (dolist (param params)
    (when (typep param 'data-parameter/specified-length)
      (let ((size (pi-size-parameter param)))
	(when (symbolp size)
	  (let ((q (or (find size params :key #'pi-name)
		       (error "referenced size parameter not found: ~A"
			      size))))
	    (setf (pi-size-parameter param) q)
	    (setf (size-parameter-p q) t)))))))

(defmethod initialize-instance :after ((instance frame-info) &key)
  (link-size-parameters! (fi-parameters instance))
  (check-offsets (fi-parameters instance))
  (setf (fi-explicit-parameters instance)
	(remove-if #'size-parameter-p (fi-parameters instance))))

;;;;
;;;; PARAMETER-INFO
;;;; Describes an individual parameter: It's name, type, offset, etc.
;;;;

(defclass parameter-info ()
  ((name   :initarg :name   :accessor pi-name)
   (offset :initarg :offset :accessor pi-offset)))

(defmethod print-object ((instance parameter-info) stream)
  (print-unreadable-object (instance stream :type t)
    (format stream "named ~A at offset ~A"
	    (pi-name instance)
	    (pi-offset instance))))

(defclass nbytes-mixin ()
  ((nbytes :initarg :nbytes :accessor pi-nbytes)))

(defclass string-parameter (nbytes-mixin parameter-info)
  ())

(defclass status-parameter (parameter-info) ())

(defmethod size-parameter-p ((parameter parameter-info))
  nil)

(defclass numeric-parameter (parameter-info)
  ((size-parameter-p :initform nil :accessor size-parameter-p)))
(defclass ubyte-parameter (numeric-parameter) ())
(defclass uword-parameter (numeric-parameter) ())
(defclass ulong-parameter (numeric-parameter) ())
(defclass sbyte-parameter (numeric-parameter) ())
(defclass sword-parameter (numeric-parameter) ())
(defclass slong-parameter (numeric-parameter) ())

(defclass data-parameter (parameter-info) ())
(defclass variable-length-data-parameter (data-parameter) ())

(deftype fixed-length-parameter ()
  `(and parameter-info (not variable-length-data-parameter)))

;;; We distinguish between subtly different kinds of length encoding for
;;; for byte data found in the protocol:

(defclass data-parameter/fixed-length (nbytes-mixin data-parameter)
  ()
  (:documentation
   "A data parameter which is specified as fixed-length in the protocol.
    This kind of parameter does not have a size field on the wire."))

(defclass data-parameter/implicit-length (variable-length-data-parameter)
  ()
  (:documentation
   "A data parameter which is variable-length, but does not have a size field
    on the wire.  This type of parameter can occur only at the end of a
    frame."))

(defclass data-parameter/specified-length (variable-length-data-parameter)
  ((size-parameter :initarg :size-parameter :accessor pi-size-parameter)
   (adjust-size    :initarg :adjust-size    :accessor pi-adjust-size))
  (:documentation
   "A data parameter which is variable-length, but and stores its size
    (plus the value of adjust-size) in a second parameter, the size-parameter,
    which must precede the data. "))

(defun parse-parameter-specs (specs)
  (loop :for (name spec) :on specs :by #'cddr
        :collect (parse-parameter-spec name spec)))

(defun parse-parameter-spec (name spec)
  (destructuring-bind (type offset &rest keys) spec
    (multiple-value-bind (class initargs)
	(case type
	  (string
	   (destructuring-bind (&key size) keys
	     (values 'string-parameter (list :nbytes size))))
	  (data
	   (destructuring-bind (&key size adjust-size) keys
	     (etypecase size
	       ((eql :implicit)
		(assert (null adjust-size))
		'data-parameter/implicit-length)
	       (integer
		(assert (null adjust-size))
		(values 'data-parameter/fixed-length
			(list :nbytes size)))
	       (symbol
		(values 'data-parameter/specified-length
			(list :size-parameter size
			      :adjust-size (or adjust-size 0)))))))
	  (t
	   (when keys
	     (error "keys specified, but not expected for parameter: ~A" spec))
	   (case type
	     (status (assert (eql offset 2)) 'status-parameter)
	     (ubyte 'ubyte-parameter)
	     (uword 'uword-parameter)
	     (ulong 'ulong-parameter)
	     (sbyte 'sbyte-parameter)
	     (sword 'sword-parameter)
	     (slong 'slong-parameter)
	     (t (error "Unknown type ~S in specification" spec)))))
      (apply #'make-instance
	     class
	     :name name
	     :offset offset
	     initargs))))

(defun fi-nbytes (fi arguments)
  (+ (n-static-bytes fi)
     (loop for arg in arguments
	   for param in (fi-explicit-parameters fi)
	   when (typep param 'variable-length-data-parameter)
	   summing (length arg))))

(defun n-static-bytes (fi)
  "Cached length of the frame, excluding variable-length DATA, but including
   fixed-length DATA (and, of course, also including the length parameter
   which DATA refers to, if any)."
  (or (%n-static-bytes fi)
      (setf (%n-static-bytes fi)
	    (max 2 (reduce #'max
			   (fi-parameters fi)
			   :key (lambda (p)
				  (etypecase p
				    (fixed-length-parameter (pi-end p))
				    (variable-length-data-parameter -1)))
			   :initial-value 0)))))

;;;;
;;;; command definition macros 
;;;;

(defun argnames-from-args (args)
  ;; hack: collect the argument names, expect for data size parameters
  (let ((sizes (loop
		  :for spec :in (cdr args) :by #'cddr
		  :for size = (getf (cddr spec) :size)
		  :when (and size (symbolp size))
		  :collect size)))
    (loop
       :for key :in args :by #'cddr
       :unless (member key sizes)
       :collect key)))

;; TRANSLATING THE FORMS INTO COMMANDS
(defun create-nxt-command-form (name type-code command-code &rest args)
  (let ((argnames (argnames-from-args args)))
    `(progn
       (setf (fp-request (ensure-frame-pair ',name ',type-code ',command-code))
	     (parse-frame-info ',args))
       (defun ,name (&key ,@argnames
		          (nxt *nxt*)
		          (onewayp nil)
		          (return-style :alist)
		          check-status)
	 (perform-command nxt
			  (load-time-value (find-frame-pair ',name))
			  onewayp
			  return-style
			  check-status
			  (list ,@argnames))))))

(defmacro def-nxt-command (name name-code type-code &rest rest)
  "Creates the command function `name' which is a nxt command with the
two command identification bytes `type-code' and `command-code'.
The `args' contains key value pairs that should be taken by the function together
with a type specifier.  Type specifiers are lists of the form (`type' `position' rest).
The type is one of

  ubyte   - unsigned byte
  uword   - unsigned word (2 bytes)
  ulong   - unsigned long (4 bytes)
  string  - has additional parameter, `final-position'."
  ;; Note: The EXPORT was commented out when I first came across this code.
  ;; We could actually EXPORT these symbols programmatically, but it would
  ;; have to be done at FASL load time, not macro ocpmilation time:
;  (export name)
  (apply #'create-nxt-command-form name name-code type-code rest))

(defmacro def-reply-package (code &rest rest)
  "Creates a `parse-nxt-reply' specialized `(eql code)' to parse a nxt reply package.
The method `(parse-nxt-reply ((code (eql code)) (data vector)) ...)' 
..."
  `(setf (fp-reply (or (lookup-command-code ',code)
		       (error "reply defined before its command: #x~2,'0X" ',code)))
	 (parse-frame-info '(status (status 2) ,@rest))))


;;;;
;;;; Status table
;;;;

(defparameter *status-values* '(0 :success 
				;; Direct commands
				#x20 :pending-communication-transaction-in-progress
				#x40 :speficied-mailbox-queue-is-empty
				#xbd :request-failed
				#xbe :unknown-command-opcode
				#xbf :insane-packet
				#xc0 :dta-contains-out-of-range-values
				#xdd :communication-bus-error
				#xde :no-free-memory-in-communication-buffer
				#xdf :specified-channel/connection-is-not-valid
				#xe0 :specified-channel/connection-not-configured-or-busy
				#xec :no-active-program
				#xed :illegal-size-specified
				#xee :illegal-mailbox-queue-id-specified
				#xef :attempted-to-access-invalid-field-of-structure
				#xf0 :bad-input-or-output-specified
				#xfb :insufficient-memory-available
				#xff :bad-arguments

				;; Communication protocol
				#x81 :no-more-handles
				#x82 :no-space
				#x83 :no-more-files
				#x84 :end-of-file-expected
				#x85 :end-of-file
				#x86 :not-a-linear-file
				#x87 :file-not-found
				#x88 :handle-all-ready-closed
				#x89 :no-linear-space
				#x8a :undefined-error
				#x8b :file-busy
				#x8c :no-write-buffers
				#x8d :append-not-possible
				#x8e :file-is-full
				#x8f :file-exists
				#x90 :module-not-found
				#x91 :out-of-boundary
				#x92 :illegal-file-name
				#x93 :illegal-handle))


;;;;
;;;; Send commands and read replies:
;;;;

(defun perform-command (nxt fp onewayp return-style expected-status args)
  (when (and expected-status onewayp)
    (error "cannot check status of a oneway command"))
  (let ((tc (fp-type-code fp))
	(cc (fp-command-code fp)))
    (write-request nxt tc cc (fp-request fp) onewayp args)
    (unless onewayp
      (read-reply/checked nxt cc (fp-reply fp) return-style expected-status))))

(defun read-reply/checked (nxt cc fi return-style expected-status)
  (let ((alist (read-reply nxt cc fi))
	(expected-status (case expected-status
			 ((t) :success)
			 (t expected-status))))
    (when (and expected-status
	       (not (eq (cdar alist) expected-status)))
      (error "expected status ~A but received ~A"
	     expected-status
	     (cdar alist)))
    (ecase return-style
      (:alist alist)
      (:values (apply #'values (mapcar #'cdr alist))))))

(defun write-request (nxt tc cc fi onewayp args)
  (write-to-nxt nxt (encode-frame (logior tc (if onewayp #x80 0)) cc fi args)))

(defun encode-frame (tc cc fi args)
  (let ((data-vector (make-array (fi-nbytes fi args)
				 :element-type '(unsigned-byte 8)
				 ;; "offensive programming": Initialize
				 ;; with a non-zero byte which is easy to
				 ;; spot when debugging:
				 :initial-element #xaa)))
    (write-ubyte-to-command data-vector tc 0)
    (write-ubyte-to-command data-vector cc 1)
    (loop :for arg :in args
          :for param :in (fi-explicit-parameters fi)
          :do (encode-parameter param arg data-vector)) 
    data-vector))

(defun read-reply (nxt cc fi)
  (let* ((buf (read-from-nxt nxt))
	 (tc2 (elt buf 0))
	 (cc2 (elt buf 1)))
    (unless (and (eql 2 tc2) (eql cc cc2))
      (cerror "try to recover"
	      "received unexpected reply ~D/~D instead of 2/~D"
	      tc2 cc2 cc)
      (setf fi (fp-reply (lookup-command-code cc2))))
    (loop
       :for param :in (fi-parameters fi)
       :collect (cons (pi-name param) (decode-parameter param buf)))))


;;;;
;;;; encoding and decoding
;;;;

(defgeneric pi-nbytes (param)
  (:method ((p status-parameter)) 1)
  (:method ((p ubyte-parameter)) 1)
  (:method ((p uword-parameter)) 2)
  (:method ((p ulong-parameter)) 4)
  (:method ((p sbyte-parameter)) 1)
  (:method ((p sword-parameter)) 2)
  (:method ((p slong-parameter)) 4)

  ;; DATA
  (:method ((p data-parameter/implicit-length))
    :unknown)
  (:method ((p data-parameter/specified-length))
    :unknown)

  ;; (:method ((p nbytes-mixing)) ;STRING- and DATA-PARAMETER/FIXED-LENGTH
  ;;   ;; length is known, but PI-NBYTES is a slot reader already, so we
  ;;   ;; do not need to define a method here
  ;;   )

  (:documentation
   "Returns the length in bytes occupied this parameter. The exception is if
    a 'data' spec is present.  The data is variable length and is not
    accounted for.  However, if the size of the data is stored in the frame,
    the ubyte where the length of the data is stored IS taken into account."))

(defun +& (a b)
  "adds numbers, but lets symbols fall through"
  (cond ((symbolp a) a)
	((symbolp b) b)
	(t (+ a b))))

(defun pi-end (p)
  "The offset of the first byte following the parameter.
   Returns :unknown if the end is not known (for a DATA-PARAMETER)."
  (+& (pi-offset p) (pi-nbytes p)))

(defun pi-last-byte (p)
  "The offset of the parameter's last byte, i.e. the byte preceding PI-END.
   Returns :unknown if the end is not known (for a DATA-PARAMETER)."
  (+& (pi-end p) -1))

(defgeneric encode-parameter (param val into-vector)

  (:method ((param ubyte-parameter) val vector)
    (check-type val (unsigned-byte 8))
    (write-ubyte-to-command vector val (pi-offset param)))

  (:method ((param sbyte-parameter) val vector)
    (check-type val (signed-byte 8))
    (write-ubyte-to-command vector (logand val #xff) (pi-offset param)))

  (:method ((param status-parameter) val vector)
    (error "status parameter expected only in replies"))

  (:method ((param string-parameter) val vector)
    (let* ((bytes (babel:string-to-octets
		   val
		   :encoding (babel-encodings:get-character-encoding :ascii)))
	   (actual (length bytes))
	   (start (pi-offset param))
	   (end (pi-end param)))
      (assert (< (1+ actual) end))
      (replace vector bytes :start1 start)
      (fill vector 0 :start (+ start actual) :end end)))

  (:method ((param uword-parameter) val vector)
    (check-type val (unsigned-byte 16))
    (write-uword-to-command vector val (pi-offset param)))

  (:method ((param sword-parameter) val vector)
    (check-type val (signed-byte 16))
    (write-uword-to-command vector (logand val #xffff) (pi-offset param)))

  (:method ((param ulong-parameter) val vector)
    (check-type val (unsigned-byte 32))
    (write-ulong-to-command vector val (pi-offset param)))

  (:method ((param ulong-parameter) val vector)
    (check-type val (signed-byte 32))
    (write-ulong-to-command vector (logand val #xffffffff) (pi-offset param)))

  (:method ((param data-parameter/fixed-length) val vector)
    (assert (eql (length val) (pi-nbytes param)))
    (replace vector val :start1 (pi-offset param)))

  (:method ((param data-parameter/implicit-length) val vector)
    (replace vector val :start1 (pi-offset param)))

  (:method ((param data-parameter/specified-length) val vector)
    (encode-parameter (pi-size-parameter param)
		      (+ (length val) (pi-adjust-size param))
		      vector)
    (replace vector val :start1 (pi-offset param))))

(defun sign-extend (bits value)
  (if (logbitp (1- bits) value)
      (dpb value (byte bits 0) -1)
      value))

(defgeneric decode-parameter (param vector)

  (:method ((param ubyte-parameter) vector)
    (read-ubyte-from-reply vector (pi-offset param)))

  (:method ((param sbyte-parameter) vector)
    (sign-extend 8 (read-ubyte-from-reply vector (pi-offset param))))

  (:method ((param status-parameter) vector)
    (let ((code (read-ubyte-from-reply vector (pi-offset param))))
      (getf *status-values* code code)))

  (:method ((param string-parameter) vector)
    (let ((offset (pi-offset param))
	  (end (pi-end param))
	  (enc (babel-encodings:get-character-encoding :ascii)))
      (babel:octets-to-string vector
			      :start offset
			      :end (min end
					(position 0 vector :start offset))
			      :encoding enc)))

  (:method ((param uword-parameter) vector)
    (read-uword-from-reply vector (pi-offset param)))

  (:method ((param sword-parameter) vector)
    (sign-extend 16 (read-uword-from-reply vector (pi-offset param))))

  (:method ((param ulong-parameter) vector)
    (read-ulong-from-reply vector (pi-offset param)))

  (:method ((param slong-parameter) vector)
    (sign-extend 32 (read-ulong-from-reply vector (pi-offset param))))

  (:method ((param data-parameter/fixed-length) vector)
    (subseq vector (pi-offset param) (pi-end param)))
  
  (:method ((param data-parameter/implicit-length) vector)
    ;; At this point you might ask: isn't there padding going on with USB?
    ;; Yes, but this method never actually gets called: This kind of parameter
    ;; gets only encoded by us, never decoded.
    (warn "unexpectedly decoding a DATA-PARAMETER/IMPLICIT-LENGTH")
    (subseq vector (pi-offset param)))
  
  (:method ((param data-parameter/specified-length) vector)
    (let ((offset (pi-offset param))
	  (nbytes (- (decode-parameter (pi-size-parameter param) vector)
		     (pi-adjust-size param))))
      (subseq vector offset (+ offset nbytes)))))

;;;; Introspection

(defun list-all-commands (&optional apropos)
  (sort (loop for fp across *code-to-info-table*
	   when (and fp
		     (or (null apropos)
			 (search (string-upcase apropos)
				 (string (fp-name fp)))))
	   collect fp)
	#'string-lessp
	:key (alexandria:compose #'string #'fp-name)))

(defun show-commands (&optional apropos)
  (dolist (fp (list-all-commands apropos))
    (format t "=========================================================~%")
    (show-command fp)
    (terpri))
  nil)

(defun show-command (fp)
  (format t "Frame pair ~A~%" (fp-name fp))
  (format t "  Type code    #x~2,'0X~%" (fp-type-code fp))
  (format t "  Command code #x~2,'0X~%" (fp-command-code fp))
  (format t "~%  Request: ")
  (show-frame-info (fp-request fp))
  (format t "  Reply:   ")
  (show-frame-info (fp-reply fp)))

(defun variable-length-frame-p (fi)
  (find-if (lambda (x)
	     (typep x 'variable-length-data-parameter))
	   (fi-parameters fi)))

(defun show-frame-info (fi)
  (let ((variable-length-p (variable-length-frame-p fi))
	(params (fi-parameters fi))
	(n (n-static-bytes fi)))
    (if variable-length-p
	(format t "Variable-length frame of size >= ~D" n)
	(format t "Fixed-length frame of size ~D" n))
    (format t " with ~D parameter~:P~%" (length params))
    (when params
      (format t "      offset, name                  length          ~%")
      (format t "      ---------------------------------------------------~%")
      (dolist (p params)
	(format t "      ~2D ~A ~37T~2D"
		(pi-offset p)
		(pi-name p)
		(pi-nbytes p))
	(when (typep p 'data-parameter/specified-length)
	  (format t ", size: ~A~@[, adjust: ~D~]"
		  (pi-name (pi-size-parameter p))
		  (and (not (zerop (pi-adjust-size p)))
		       (pi-adjust-size p))))
	(format t "~16T~A~%" (etypecase p
			       (status-parameter "(status byte)")
			       (string-parameter "[string]")
			       (ubyte-parameter  "[ubyte]")
			       (uword-parameter  "[uword]")
			       (ulong-parameter  "[ulong]")
			       (data-parameter   "[data]"))))
      (terpri))))
