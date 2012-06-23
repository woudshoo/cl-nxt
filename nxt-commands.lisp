;;; Copyright (c) 2010-2011 by Willem Rein Oudshoorn
;;;
;;; Licensed under the BSD License, see accompanied LICENSE file
;;;
(in-package :nxt)

(clear-all)

;;; Note: The comment for CHECK-OFFSETS explains why we list byte offsets
;;; redundantly in this file.

(def-nxt-command nxt-open-read #x01 #x80 file-name (string 2 :size 20))
(def-reply-package #x80 handle (ubyte 3) file-size (ulong 4))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-open-write #x01 #x81
		 file-name (string 2 :size 20)
		 file-size (ulong 22))
(def-reply-package #x81 handle (ubyte 3))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-read #x01 #x82 handle (ubyte 2) nr-of-bytes (uword 3))
(def-reply-package #x82 handle (ubyte 3)
		        amount-read (uword 4)
			data (data 6 :size amount-read))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-write #x01 #x83 
		 handle (ubyte 2) 
		 data (data 3 :size :implicit))
(def-reply-package #x83 handle (ubyte 3)  amount-written-to-flash (uword 4))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-close #x01 #x84 handle (ubyte 2))
(def-reply-package #x84 handle (ubyte 3))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-delete #x01 #x85 
		 file-name (string 2 :size 20))
(def-reply-package #x85 file-name (string 3 :size 20))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-find-first #x01 #x86
		 file-name (string 2 :size 20))
(def-reply-package #x86 handle (ubyte 3)
		   file-name (string 4 :size 20)
		   file-size (ulong 24))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-find-next #x01 #x87 handle (ubyte 2))
(def-reply-package #x87 handle (ubyte 3)
		        file-name (string 4 :size 20)
			file-size (ulong 24))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-get-firmware-version #x01 #x88)
(def-reply-package #x88 protocol-version-minor (ubyte 3)
		        protocol-version-major (ubyte 4)
                        firmware-version-minor (ubyte 5)
                        firmware-version-major (ubyte 6))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-get-device-info      #x01 #x9b)
(def-reply-package #x9b
    nxt-name (string 3 :size 15)
    bluetooth-address (string 18 :size 7)
    bluetooth-signal-strength (ulong 25)
    free-flash (ulong 29))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-request-first-module    #x01 #x90
		 resource-name (string 2 :size 20))
(def-reply-package #x90 handle (ubyte 3)
		   module-name (string 4 :size 20)
		   module-id (ulong 24)
		   module-size (ulong 28)
		   io-map-size (uword 32))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-request-next-module #x01 #x91 handle (ubyte 2))
(def-reply-package #x91 handle (ubyte 3)
		   module-name (string 4 :size 20)
		   module-id (ulong 24)
		   module-size (ulong 28)
		   io-map-size (uword 32))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-close-module-handle #x01 #x92 handle (ubyte 2))
(def-reply-package #x92 handle (ubyte 3))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-start-program #x00 #x00
		 file-name (string 2 :size 20))
(def-reply-package #x00)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-stop-program  #x00 #x01)
(def-reply-package #x01)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-play-sound-file #x00 #x02
		 loop (ubyte 2)
		 file-name (string 3 :size 20))
(def-reply-package #x02)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-play-tone #x00 #x03 frequency (uword 2) duration (uword 4))
(def-reply-package #x03)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-set-output-state #x00 #x04 
		 output-port (ubyte 2) 
		 power-set-point (ubyte 3)
		 mode (ubyte 4)
		 regulation-mode (ubyte 5)
		 turn-ratio (ubyte 6) 
		 run-state (ubyte 7)
		 tacho-limit (ulong 8))
(def-reply-package #x04)		 
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-set-input-mode #x00 #x05
		 input-port (ubyte 2)
		 sensor-type (ubyte 3)
		 sensor-mode (ubyte 4))
(def-reply-package #x05)		 
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-get-output-state #x00 #x06
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
    rotation-count (ulong 21)) ;; should be slong
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-get-input-values #x00 #x07 input-port (ubyte 2))
(def-reply-package #x07
    input-port (ubyte 3)
    valid (ubyte 4)
    calibrated (ubyte 5)
    sensor-type (ubyte 6)
    sensor-mode (ubyte 7)
    raw-a/d-value (uword 8)
    normalized-a/d-value (uword 10)
    scaled-value (uword 12)  ;; should be sword
    calibrated-value (uword 14)) ;; should be sword
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-reset-input-scaled-value #x00 #x08 input-port (ubyte 2))
(def-reply-package #x08)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-message-write #x00 #x09 
		 inbox (ubyte 2)
		 message-size (ubyte 3)
		 message-data (data 4 :size message-size)) 
(def-reply-package #x09)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-reset-motor-position #x00 #x0a output-port (ubyte 2) relative (ubyte 3))
(def-reply-package #x0a)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-get-battery-level #x00 #x0b)
(def-reply-package #x0b voltage (uword 3))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-stop-sound-playback #x00 #x0c)
(def-reply-package #x0c)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-keep-alive #x00 #x0d)
(def-reply-package #x0d sleep-time-limit (ulong 3))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-ls-get-status #x00 #x0e port (ubyte 2))
(def-reply-package #x0e bytes-ready (ubyte 3))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-ls-write #x00 #x0f
		 port (ubyte 2)
 		 tx-data-length (ubyte 3)
 		 rx-data-length (ubyte 4)
		 tx-data (data 5 :size tx-data-length))
(def-reply-package #x0f)
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-ls-read #x00 #x10 port (ubyte 2))
(def-reply-package #x10
		   rx-data (data 3 :size 17))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-get-current-program-name #x00 #x11)
(def-reply-package #x11 file-name (string 3 :size 20))
;;--------------------------------------------------------------------------------
(def-nxt-command nxt-message-read #x00 #x13
		 remote-inbox (ubyte 2)
		 local-inbox (ubyte 3)
		 remove (ubyte 4))
(def-reply-package #x13 
    local-inbox (ubyte 3) 
    message-size (ubyte 4) 
    message-data (string 5 :size 59))
