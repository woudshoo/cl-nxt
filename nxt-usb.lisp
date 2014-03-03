;;;; We use libusb 1.0 (and not 0.1!) for USB communication to the NXT,
;;;; because its structs are simpler, avoiding the need for cffi-grovel.
;;;;
;;;; You might ask: Why aren't we just depending on usbserial, so that we
;;;; would connect to /dev/ttyUSB, similarly as with Bluetooth?
;;;;
;;;; It turns out that ttyUSB doesn't even work for LEGO.  We can connect,
;;;; but then something goes wrong when sending and receiving data.  Lego
;;;; wants USB "bulk" data transfers, where each LEGO protocol message is
;;;; one bulk transfer, and I suppose usbserial doesn't do it the right way.
;;;; As a result, I can sometimes send commands, but usually the reply
;;;; from the brick gets lost...
;;;;
;;;; Also, libusb is much nicer anyway, because we can enumerate and
;;;; autodetect devices, and the user doesn't have to fool around with
;;;; modprobe.

(in-package :nxt)

;;;;
;;;; FFI
;;;;

(cffi:define-foreign-library :libusb
  (:darwin "libusb-1.0.0.dylib")
  (:unix "libusb-1.0.so.0"))

(defmacro defcfun (name ret &rest args)
  (let ((sym (gensym))
	(argnames (mapcar #'car args))
	;; By libusb's API convention, any function returning int uses
	;; negative values to indicate errors, and non-negative values for
	;; success.
	(checker
	 (case ret
	   (:int `(lambda (ret)
		    (when (minusp ret)
		      (error "~(~A~) failed: returned ~A" ',name ret))
		    ret))
	   (t 'progn))))
    `(progn
       ;; Since we never export FFI functions and use them only internally,
       ;; I prefer the naming convention where it's obvious from the
       ;; function name that it's an FFI function, so suppress the automagic
       ;; translation from underscore to dash:
       (cffi:defcfun (,sym ,(string-downcase name)) ,ret
	 ,@args)
       (defun ,name (,@argnames)
	 (,checker (,sym ,@argnames))))))

(defcfun libusb_init :int
  (&context :pointer))

(defcfun libusb_exit :void
  (context :pointer))

(defcfun libusb_open_device_with_vid_pid :pointer
  (context :pointer)
  (vendor :short)
  (device :short))

(defcfun libusb_close :void
  (device :pointer))

(defcfun libusb_claim_interface :int
  (device :pointer)
  (iface :int))

(defcfun libusb_release_interface :int
  (device :pointer)
  (iface :int))

(defcfun libusb_set_configuration :int
  (device :pointer)
  (configuration :int))

(defcfun libusb_bulk_transfer :int
  (device :pointer)
  (endpoint :unsigned-char)
  (data :pointer)
  (length :int)
  (transferred :pointer)
  (timeout :unsigned-int))


;;;;
;;;; Initialization
;;;;

(defvar *libusb-context* nil)
(defvar *libusb-loaded-p* nil)

(defun ensure-libusb-loaded ()
  (unless *libusb-loaded-p*
    (cffi:load-foreign-library :libusb)
    (setf *libusb-loaded-p* t)))

(defun ensure-libusb-initialized ()
  (unless *libusb-context*
    (ensure-libusb-loaded)
    (cffi:with-foreign-object (&context :pointer)
      (libusb_init &context)
      (setf *libusb-context* (cffi:mem-ref &context :pointer)))))

(defun exit-libusb ()
  (unless *libusb-context*
    (error "cannot exit what hasn't been init'ed"))
  (libusb_exit *libusb-context*)
  (setf *libusb-context* nil))

;;;;
;;;; NXT
;;;;

(defclass usb-nxt (nxt)
  ((handle :initarg :handle
	   :accessor handle)
   (buf :initarg :buf
	:accessor buf)))

(defconstant +usb-configuration+ 1)
(defconstant +usb-interface+ 0)
(defconstant +usb-send-endpoint+ 1)
(defconstant +usb-recv-endpoint+ 130)
(defconstant +usb-maximum-frame-size+ 64)

(defun find-usb-nxt ()
  (ensure-libusb-initialized)
  (let ((handle (libusb_open_device_with_vid_pid *libusb-context*
						 #x0694
						 #x0002)))
    (cond
      ((cffi-sys:null-pointer-p handle)
	nil)
      (t
       (libusb_set_configuration handle +usb-configuration+)
       (libusb_claim_interface handle +usb-interface+)
       (make-instance 'usb-nxt
		      :handle handle
		      :buf (static-vectors:make-static-vector
			    +usb-maximum-frame-size+))))))

(defmethod close-nxt ((nxt usb-nxt))
  ;; release the handle
  (handler-case
      (libusb_release_interface (handle nxt) +usb-interface+)
    (error (c) ;; continue even if something went wrong (USB unplugged, etc.)
      (warn "~A" c)))
  (libusb_close (handle nxt))
  (setf (handle nxt) nil)
  ;; release foreign memory
  (static-vectors:free-static-vector (buf nxt))
  (setf (buf nxt) nil))


;;;; Reading and writing.
;;;;
;;;; The USB protocol does not have a length prefix (unlike Bluetooth).
;;;; It also does not need arbitrary delays within the raw protocol
;;;; (except for high-level contraints as in motor control.)

;;; we need to specify a timeout in case the call blocks.  A few seconds
;;; should be a robust value: USB communication is normally
;;; "instantaneous" (<= 1 ms).  So a large timeout value doesn't affect
;;; performance.  And if we exceed even this value, something must be truly
;;; wrong, so an error is appropriate.
(defvar *usb-timeout* 10000)		;ms

(defmethod write-to-nxt ((nxt usb-nxt) data)
  (let ((n (length data))
	(buf (buf nxt)))
    (replace buf data)
    (cffi:with-foreign-object (&transferred :int)
      (libusb_bulk_transfer (handle nxt)
			    +usb-send-endpoint+
			    (static-vectors:static-vector-pointer buf)
			    n
			    &transferred
			    *usb-timeout*) 
      (assert (eql n (cffi:mem-ref &transferred :int)))
      t)))

;; Note: These values are padded with junk.  Ideally we would trim them,
;; but IIUC there are operations (or at least one) that has variable-length
;; data.  FIXME.
(defmethod read-from-nxt ((nxt usb-nxt))
  (let ((buf (buf nxt)))
    (cffi:with-foreign-object (&transferred :int)
      (libusb_bulk_transfer (handle nxt)
			    +usb-recv-endpoint+
			    (static-vectors:static-vector-pointer buf)
			    +usb-maximum-frame-size+
			    &transferred
			    *usb-timeout*)
      (subseq buf 0 (cffi:mem-ref &transferred :int)))))
