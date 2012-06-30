;;;; A few utility functions to query various sensors types, in particular
;;;; the digital I2C sensors, where well-known addresses from sensor-specific
;;;; documentation are required for use.
;;;;
;;;; Includes support for many (not yet all) LEGO sensors, and various
;;;; thirdparty sensors from HiTechnic.

(in-package :nxt)

(defconstant +no-sensor+      0)
(defconstant +switch+         1)
(defconstant +temperature+    2)
(defconstant +reflection+     3)
(defconstant +angle+          4)
(defconstant +light-active+   5)
(defconstant +light-inactive+ 6)
(defconstant +sound-db+       7)
(defconstant +sound-dba+      8)
(defconstant +custom+         9)
(defconstant +lowspeed+      10)
(defconstant +lowspeed-9v+   11)

(defconstant +raw-mode+            #x00)
(defconstant +boolean-mode+        #x20)
(defconstant +transition-cnt-mode+ #x40)
(defconstant +period-counter-mode+ #x60)
(defconstant +pct-full-scale-mode+ #x80)
(defconstant +celsius-mode+        #xa0)
(defconstant +fahrenheit-mode+     #xc0)
(defconstant +anglestep-mode+      #xe0)
(defconstant +slope-mask+          #x1f)
(defconstant +mode-mask+           #xe0)

(defun compute-timeout (s)
  (when s
    (+ (get-internal-real-time) (* s internal-time-units-per-second))))

(defun timeoutp (abs-timeout)
  (and abs-timeout (> (get-internal-real-time) abs-timeout)))

(defun get-valid-input-values (port &key (nxt *nxt*) (timeout 1))
  ;; Repeat until we get valid values, or a timeout is reached.
  ;; FIXME: I didn't see this problem (not since implementing bluetooth
  ;; timeouts fixed incorrect status returns from this call anyway.)
  ;; How and when is it actually important?
  (let ((abs-timeout (compute-timeout timeout)))
    (loop
       (let ((x (nxt-get-input-values :input-port port :nxt nxt)))
	 (when (or (plusp (cdr (assoc 'valid x)))
		   (timeoutp abs-timeout))
	   (return x))))))

(defun get-calibrated-value (port &key (nxt *nxt*))
  (cdr (assoc 'calibrated-value (get-valid-input-values port :nxt nxt))))

(defun get-scaled-value (port &key (nxt *nxt*))
  (cdr (assoc 'scaled-value (get-valid-input-values port :nxt nxt))))

(defun get-normalized-value (port &key (nxt *nxt*))
  (cdr (assoc 'normalized-a/d-value (get-valid-input-values port :nxt nxt))))


;;;; LEGO switch (touch) sensor

(defun switch-on (port &key (nxt *nxt*))
  (nxt-set-input-mode :input-port port
		      :nxt nxt
		      :check-status t
		      :sensor-type +switch+
		      :sensor-mode +raw-mode+))

(defun get-switch (port &key (nxt *nxt*))
  (< (get-normalized-value port :nxt nxt) 500))


;;;; LEGO (not HiTechnic!) color/light detector

;; Has three LEDs to emit four kinds color, and sense brightness or color.

(defun lightdetector-on (port &key (color :full) (nxt *nxt*))
  (let ((mode (ecase color
		(:full #xd)
		(:red #xe)
		(:green #xf)
		(:blue #x10)
		((nil) #x11))))
    (nxt-set-input-mode :nxt nxt
			:check-status t
			:input-port port
			:sensor-type mode
			:sensor-mode 0)))

(defun light-off (port &key (nxt *nxt*))
  (lightdetector-on port :color nil :nxt nxt))

(defun get-brightness-or-color (port &key (nxt *nxt*))
  (let ((x (get-valid-input-values port :nxt nxt)))
    (ecase (cdr (assoc 'sensor-type x))
      (13 (cdr (assoc 'normalized-a/d-value x)))
      (t (let ((val (cdr (assoc 'scaled-value x))))
	   (case val
	     (1 :black)
	     (2 :blue)
	     (3 :green)
	     (4 :yellow)
	     (5 :red)
	     (6 :white)
	     (t val)))))))


;;;; helper stuff for I2C communication

;; FIXME: where on these constants specified?
;; Not the usual "Appendix 1-3" spec AFAICT.
(defconstant +default-i2c-address+ #x02)
(defconstant +state-command+       #x41)
(defconstant +single-shot+         #x01)
(defconstant +continuous+          #x02)

(defun ub8-vector (&rest args)
  (make-array (length args)
	      :element-type '(unsigned-byte 8)
	      :initial-contents args))

;;;; version numbers

;;; Example output:
;;;
;; Scanning sensors, please wait...
;; 0 LEGO     Sonar    86
;; 1 <not an I2C sensor>
;; 2 HITECHN  AnglSns  V2.4
;; 3 HITECHN  Compass  V2.1

(defun enumerate-digital-sensors (&key (nxt *nxt*))
  (format t "Scanning sensors, please wait...~%")
  (dotimes (i 4)
    (multiple-value-bind (version manu type)
	(probe-digital-sensor i :nxt nxt)
      (if (or version manu type)
	  (format t "~D ~8A ~8A ~8A~%" i manu type version)
	  (format t "~D <not an I2C sensor>~%" i)))))

(defun probe-digital-sensor (port &key (nxt *nxt*))
  (nxt-set-input-mode :nxt nxt
		      :check-status t
		      :input-port port
		      :sensor-type +lowspeed-9v+
		      :sensor-mode +raw-mode+)
  (sleep 0.1)
  (flet ((stringify (bytes)
	   (let ((n (or (position 0 bytes)
			(position 255 bytes)
			(length bytes))))
	     (if (plusp n)
		 (babel:octets-to-string bytes :end n)
		 nil))))
    (handler-bind ((warning #'muffle-warning))
      (let* ((babel:*default-character-encoding*
	      (babel-encodings:get-character-encoding :latin1))
	     (version (or (stringify (read-i2c port #x7 2 #x01 :nxt nxt))
			  (let ((bytes (read-i2c port #x1 2 #x00 :nxt nxt)))
			    (and (plusp (length bytes))
				 (elt bytes 0)))))
	     (manufacturer (stringify (read-i2c port 7 2 8 :nxt nxt)))
	     (type (stringify (read-i2c port 7 2 16 :nxt nxt))))
	(values version manufacturer type)))))


;;;; LEGO ultrasonic range finder


(defun ultrasonic-on (port &key single-shot-p (nxt *nxt*))
  (when single-shot-p
    (warn "sorry, functions to actually retrieve single shot batches not implemented yet"))
  (nxt-set-input-mode :nxt nxt
		      :check-status t
		      :input-port port
		      :sensor-type +lowspeed-9v+
		      :sensor-mode +raw-mode+)
  ;; FIXME: these inline sleep calls are really beginning to be annoying.
  ;; While it would certainly be a valid choice for an NXT library to be
  ;; blocking, such an API is very suboptimal.  There no good reason for
  ;; the whole program to block for 50ms just because this particular
  ;; sensor needs a delay.  What we should really do is to offer a
  ;; non-blocking API for -everything- that could possibly block, as an
  ;; alternative for higher-level code which knows what other requests
  ;; to send in the meantime.
  (sleep 0.05)
  (nxt-ls-write :nxt nxt
		:onewayp t
		:port port
		:rx-data-length 0
		:tx-data (ub8-vector +default-i2c-address+
				     +state-command+
				     (if single-shot-p
					 +single-shot+
					 +continuous+)))
  (nxt-ls-get-status :port port :nxt nxt :check-status nil)
  (nxt-ls-read :port port :nxt nxt :check-status nil))

(defun get-ultrasonic (port &key (nxt *nxt*))
  (let ((data (read-i2c port 1 2 #x42 :nxt nxt)))
    (if (plusp (length data))
	(elt data 0)
	:sensor-communication-failure)))

(defun read-i2c (port rx-len device register &key (nxt *nxt*))
  (let ((timeout (compute-timeout 1))
	(retry t))
    (loop
       (when retry
	 (nxt-ls-write :nxt nxt
		       :onewayp t
		       :port port
		       :rx-data-length rx-len
		       :tx-data (ub8-vector device register))
	 (setf retry nil))
       (multiple-value-bind (status data)
	   (ls-read :nxt nxt
		    :check-status nil	;(!)
		    :port port)
	 (ecase status
	   (:pending-communication-transaction-in-progress
	    ;; just loop
	    )
	   (:communication-bus-error
	    (setf retry t))
	   (:specified-channel/connection-not-configured-or-busy
	    (warn "I2C sensor not ready or not configured?"))
	   (:success
	    (return data)))
	 (when (timeoutp timeout)
	   (warn "timeout; returning possibly(?) invalid I2C data")
	   (return data))))))

;;;; HiTechnic NAA1030 Angle Sensor (Tacho)

(defun angle-sensor-on (port &key (nxt *nxt*))
  (nxt-set-input-mode :nxt nxt
		      :check-status t
		      :input-port port
		      :sensor-type +lowspeed-9v+
		      :sensor-mode +raw-mode+)
  (sleep 0.05)				;needed?
  (nxt-ls-write :nxt nxt
		:onewayp t
		:port port
		:rx-data-length 0
		:tx-data (ub8-vector +default-i2c-address+ +state-command+ 0))
  (nxt-ls-get-status :port port :nxt nxt :check-status nil)
  (nxt-ls-read :port port :nxt nxt :check-status nil))

(defun get-angle-sensor-data (port &key (nxt *nxt*))
  ;; Returns the current angle of the sensor in degrees.
  (let ((data (read-i2c port 8 2 #x42 :nxt nxt)))
    (if (eql (length data) 8)
	(values (+ (* 2 (elt data 0)) (elt data 1))
		(sign-extend 32
			     (logior (ash (elt data 2) 24)
				     (ash (elt data 3) 16)
				     (ash (elt data 4) 8)
				     (elt data 5)))
		(sign-extend 16
			     (logior (ash (elt data 6) 8) (elt data 7))))
	:sensor-communication-failure)))

(defun get-current-angle (port &key (nxt *nxt*))
  (values (get-angle-sensor-data port :nxt nxt)))

(defun get-accumulated-angle (port &key (nxt *nxt*))
  (nth-value 1 (get-angle-sensor-data port :nxt nxt)))

(defun get-rpm (port &key (nxt *nxt*))
  (nth-value 2 (get-angle-sensor-data port :nxt nxt)))


;;;; HiTechnic NAC1040 Accelerometer

(defun acceleration-sensor-on (port &key (nxt *nxt*))
  (nxt-set-input-mode :nxt nxt
		      :check-status t
		      :input-port port
		      :sensor-type +lowspeed-9v+
		      :sensor-mode +raw-mode+)
  (sleep 0.05)				;needed?
  #+nil
  (nxt-ls-write :nxt nxt
		:onewayp t
		:port port
		:rx-data-length 0
		:tx-data (ub8-vector +default-i2c-address+ +state-command+ 0))
  (nxt-ls-get-status :port port :nxt nxt :check-status nil)
  (nxt-ls-read :port port :nxt nxt :check-status nil))

(defun get-acceleration (port &key (nxt *nxt*))
  ;; => (values x=pitch y=roll z)
  (let ((data (read-i2c port 6 2 #x42 :nxt nxt)))
    (if (eql (length data) 6)
	(values
	 (sign-extend 10 (logior (ash (elt data 0) 2) (elt data 3)))
	 (sign-extend 10 (logior (ash (elt data 1) 2) (elt data 4)))
	 (sign-extend 10 (logior (ash (elt data 2) 2) (elt data 5))))
	:sensor-communication-failure)))


;;;; HiTechnic NMC1034 Compass

(defun compass-on (port &key (nxt *nxt*))
  (nxt-set-input-mode :nxt nxt
		      :check-status t
		      :input-port port
		      :sensor-type +lowspeed-9v+
		      :sensor-mode +raw-mode+)
  (sleep 0.1)
  (nxt-ls-get-status :port port :nxt nxt :check-status nil)
  (nxt-ls-read :port port :nxt nxt :check-status nil))

(defun get-compass (port &key (nxt *nxt*) (unit :degree))
  ;; Relative to north.  With :DEGREE, return the LEGO value, i.e. degrees
  ;; in the negative (clockwise) direction.  With :RADIAN, return radians in
  ;; the positive direction.
  (let ((data (read-i2c port 2 2 #x42 :nxt nxt)))
    (if (eql (length data) 2)
	(let ((deg (+ (ash (elt data 0) 1) (elt data 1))))
	  (ecase unit
	    (:degree deg)
	    (:radian (* pi 1/180 (- 360 deg)))))
	:sensor-communication-failure)))
