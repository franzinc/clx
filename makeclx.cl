(in-package :user)

(pushnew :clx-ansi-common-lisp *features*)
(load "defsystem")
(load "package")
(setq xlib::*def-clx-class-use-defclass* t)
(let ((*compile-verbose* t)
      (*compile-print* nil))
  (compile-system :clx)
  (compile-system :clx-debug))

;; how to concatenate the fasls?
(let ((fasls '("package" "excldep" "depdefs" "clx" "dependent"
	       "exclcmac" "buffer" "display" "gcontext"
	       "requests" "input" "fonts" "graphics" "text" 
	       "attributes" "translate" "keysyms" "manager"
	       "image" "resource")))
  (with-open-file (bigfasl "clxwin.fasl"
		   :element-type '(unsigned-byte 8)
		   :direction :output
		   :if-exists :error :if-does-not-exist :create)
    (let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
      (dolist (file-to-cat fasls)
	(let ((faslname (concatenate 'string file-to-cat ".fasl")))
	  (with-open-file (in faslname :element-type '(unsigned-byte 8))
	    (loop as x = (read-sequence buf in)
		until (= x 0)
		do (write-sequence buf bigfasl :end x))))))
    bigfasl))
