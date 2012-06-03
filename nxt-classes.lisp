;;;; Contains classes and infrastructure code applicable to both the Bluetooth
;;;; and the USB class.

(in-package :nxt)

(defclass nxt () ()
  (:documentation "Common superclass of BLUETOOTH-NXT and USB-NXT."))

(defvar *nxt* nil
  "Holds the default NXT instance (which functions use by default, if no
   explicit NXT parameter is given. Replaces the old *connection* variable.")

;;;; The following functions find or close an NXT, and take care of the *NXT*
;;;; default value.
;;;;
;;;; Maybe OPEN-CONNECTION isn't the ideal name anymore, because USB is
;;;; also an option.  But I'm leaving the function names unchanged for
;;;; compatibility.

(defvar *default-bluetooth-device* "/dev/tty.NXT-DevB-1")

(defun open-connection
    (&optional (look-for-types '(:usb :bluetooth))
               (device *default-bluetooth-device*))
  (when *nxt*
    (error "Default connection is already open, close it first!"))
  (setf *nxt* (find-an-nxt look-for-types device t)))

(defun close-connection ()
  (unless *nxt*
    (error "Connection is not open"))
  (unwind-protect
       (close-nxt *nxt*)
    (setf *nxt* nil)))
		      
;;;; Find an USB or a Bluetooth NXT.
;;;;
;;;; Note: Ideally we would accept a lego name here, so that we could
;;;; connect to two Legos at the same time, and (at least over USB) we would
;;;; enumerate all devices until we find the right one.  For now though,
;;;; we just use the first device we can find.
;;;;
;;;; It's generally awkward that USB is so much nicer than bluetooth in
;;;; this regard, but all other NXT libraries I have found have the same
;;;; kind of asymmetry -- Bluetooth needs a /dev/something device, whereas
;;;; USB just takes a logical brick name.

(defun find-an-nxt
    (&optional (look-for-types '(:usb :bluetooth))
               (device *default-bluetooth-device*)
               (errorp t))
  (dolist (kind look-for-types
	   (when errorp (error "no NXT found (tried ~A, ~A)"
			       look-for-types
			       device)))
    (let ((nxt
	   (ecase kind
	     (:usb (find-usb-nxt))
	     (:bluetooth (open-bluetooth-nxt device :if-does-not-exist nil)))))
      (when nxt
	(return nxt)))))
