;;;;;
;;;;; nxt-command-infrastructure
;;
;;
;; This file consists of two parts,
;;
;; 1 - infrastructure to build command vectors
;; 2 - macros to define commands + supporting functions to create the macro
;;

(in-package :nxt)

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
  "Extract unsgned byte value from the reply vector `vector' at location `location'."
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

(defun read-data-from-reply (vector start amount)
  "Reads `amount' nr of bytes from the vector at location `start'."
  (make-array amount :displaced-to vector :displaced-index-offset start))

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
(defun type-of-spec (spec)
  "Returns the type of a single variable spec for the nxt command description.
Returns one of: 'ubyte, 'uword, 'ulong, 'string."
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
  "t if the nxt command with type code `code' will generate a reply."
    (eql 0 (ldb (byte 1 7) code)))

;; TRANSLATING THE FORMS INTO COMMANDS
(defun create-nxt-command-form (name type-code command-code &rest args)
  "Creates the command function `name' which is a nxt command with the
two command identification bytes `type-code' and `command-code'.  
The `args' contains key value pairs that should be taken by the function together
with a type specifier.  Type specifiers are lists of the form (`type' `position' rest).
The type is one of 

  ubyte   - unsigned byte 
  uword   - unsigned word (2 bytes)
  ulong   - unsigned long (4 bytes)
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
  "See for the documentation the function `create-nxt-command-form'."
  (apply #'create-nxt-command-form name name-code type-code rest))

(defmacro def-reply-package (code &rest rest)
  "Creates a `parse-nxt-reply' specialized `(eql code)' to parse a nxt reply package.
The method `(parse-nxt-reply ((code (eql code)) (data vector)) ...)' 
..."
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
	       (data `(read-data-from-reply data ,(second spec) (read-uword-from-reply data ,(third spec))))
	       (t (error "Unrecognized type ~S in specification" (type-of-spec spec)))))))))


(defmethod parse-nxt-reply ((code t) (data vector))
  "Default fallback method."
  data)
