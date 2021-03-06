;;; Copyright (c) 2010-2011 by Willem Rein Oudshoorn
;;;
;;; Licensed under the BSD License, see accompanied LICENSE file
;;;
(in-package #:cl-user)

(defpackage #:nxt
  (:use #:cl)
  (:export #:nxt			;classes
	   #:usb-nxt
	   #:bluetooth-nxt

	   #:*nxt*			;default brick handling
	   #:open-connection
	   #:close-connection

	   #:find-an-nxt		;low-level opening and closing
	   #:open-bluetooth-nxt
	   #:find-usb-nxt
	   #:close-nxt
	   #:*default-bluetooth-device*

	   ;; nxt commands
	   #:nxt-open-read
	   #:nxt-open-write
	   #:nxt-read
	   #:nxt-write
	   #:nxt-close
	   #:nxt-delete
	   #:nxt-find-first
	   #:nxt-find-next
	   #:nxt-get-firmware-version
	   #:nxt-get-device-info
	   #:nxt-request-first-module
	   #:nxt-request-next-module
	   #:nxt-close-module-handle
	   #:nxt-start-program
	   #:nxt-stop-program
	   #:nxt-play-sound-file
	   #:nxt-play-tone
	   #:nxt-set-output-state
	   #:nxt-set-input-mode
	   #:nxt-get-output-state
	   #:nxt-get-input-values
	   #:nxt-reset-input-scaled-value
	   #:nxt-message-write
	   #:nxt-reset-motor-position
	   #:nxt-get-battery-level
	   #:nxt-stop-sound-playback
	   #:nxt-keep-alive
	   #:nxt-ls-get-status
	   #:nxt-ls-write
	   #:nxt-ls-read
	   #:nxt-get-current-program-name
	   #:nxt-message-read

	   ;; lower-level functions for users not wishing to go through
	   ;; the individual exported symbols above:
	   #:find-frame-pair
	   #:perform-command

	   ;; introspection
	   #:list-all-commands
	   #:show-commands
	   #:show-command

	   ;; programs
	   #:running-a-program-p
	   #:stop-current-program
	   #:start-program

	   ;; MotorControl
	   #:restart-motor-control
	   #:motor-control
	   #:stop-motor
	   #:query-motor
	   #:motor-ready-p
	   #:sleep-until-motor-ready

	   ;; sensor utilities
	   #:switch-on
	   #:get-switch

	   #:lightdetector-on
	   #:light-off
	   #:get-brightness-or-color

	   #:ultrasonic-on
	   #:get-ultrasonic

	   #:acceleration-sensor-on
	   #:get-acceleration
	   
	   #:angle-sensor-on
	   #:get-angle-sensor-data
	   #:get-current-angle
	   #:get-accumulated-angle
	   #:get-rpm

	   #:compass-on
	   #:get-compass

	   #:enumerate-digital-sensors
	   #:probe-digital-sensor))

(pushnew :nxt *features*)
