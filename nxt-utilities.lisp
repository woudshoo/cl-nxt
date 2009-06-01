(in-package :nxt)
   
(defun value-of (key assoc)
  (cdr (assoc key assoc)))

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
