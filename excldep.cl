;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- excldep.cl
;;;
;;; Copyright (c) 1987, 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

#+(version>= 5 0)
(eval-when (compile)
  (require :foreign))

(eval-when (compile load eval)
  #-(or (version>= 5 0) mswindows) (require :foreign)
  (require :process)			; Needed even if scheduler is not
					; running.  (Must be able to make
					; a process-lock.)
  )

(eval-when (load eval)
  (provide :clx))


#-(or little-endian big-endian)
(eval-when (eval compile load)
  (let ((x '#(1)))
    (if (not (eq 0 (sys::memref x
				#.(sys::mdparam 'comp::md-svector-data0-adj)
				0 :unsigned-byte)))
	(pushnew :little-endian *features*)
      (pushnew :big-endian *features*))))

#-(version>= 4 1)
(eval-when (compile load eval)
  (defmacro define-compiler-macro (&rest args)
    `(excl::defcmacro ,@args)))

(defmacro correct-case (string)
  ;; This macro converts the given string to the
  ;; current preferred case, or leaves it alone in a case-sensitive mode.
  (let ((str (gensym)))
    `(let ((,str ,string))
       (case excl::*current-case-mode*
	 (:case-insensitive-lower
	  (string-downcase ,str))
	 (:case-insensitive-upper
	  (string-upcase ,str))
	 ((:case-sensitive-lower :case-sensitive-upper)
	  ,str)))))

;; Return t if there is a character available for reading or on error,
;; otherwise return nil.
#-(version>= 6 0)
(progn

#-(or (version>= 4 2) mswindows)
(defun fd-char-avail-p (fd)
  (multiple-value-bind (available-p errcode)
      (comp::.primcall-sargs 'sys::filesys #.excl::fs-char-avail fd)
    (excl:if* errcode
       then t
       else available-p)))

#+(and (version>= 4 2) (not mswindows))
(defun fd-char-avail-p (fd)
  (excl::filesys-character-available-p fd))

#+mswindows
(defun fd-char-avail-p (socket-stream)
  (listen socket-stream))
)
#+(version>= 6 0)
(defun fd-char-avail-p (socket-stream)
  (excl::read-no-hang-p socket-stream))

(defmacro with-interrupt-checking-on (&body body)
  `(locally (declare (optimize (safety 1)))
     ,@body))

;; Read from the given fd into 'vector', which has element type card8.
;; Start storing at index 'start-index' and read exactly 'length' bytes.
;; Return t if an error or eof occurred, nil otherwise.
#-(and mswindows (version>= 6))
(defun fd-read-bytes (fd vector start-index length)
  (declare (fixnum #-mswindows fd start-index length)
	   (type (simple-array (unsigned-byte 8) (*)) vector))
  (with-interrupt-checking-on
   (do ((rest length))
       ((eq 0 rest) nil)
     (declare (fixnum rest))
     ;; added by cac 24jul99
     ;; Crude but effective way to wait for input when whole buffer
     ;; doesn't get filled all at once.  Probably should
     ;; make more robust in light of possible failing sockets.
     (loop
       (when (fd-char-avail-p fd)
	 (return)))
     (multiple-value-bind (numread errcode)
	 #-(version>= 4 2)
	 (comp::.primcall-sargs 'sys::filesys #.excl::fs-read-bytes fd vector
				start-index rest)
	 #+(version>= 4 2)
	 (excl::fill-read-buffer #-mswindows fd
				 #+mswindows (excl::stream-input-fn fd)
				 vector start-index rest)
       (declare (fixnum numread))
       (excl:if* errcode
	  then (if (not (eq errcode
			    excl::*error-code-interrupted-system-call*))
		   (return t))
	elseif (eq 0 numread)
	  then (return t)
	  else (decf rest numread)
	       (incf start-index numread))))))

#+(and mswindows (version>= 6))
(defun fd-read-bytes (fd vector start-index length)
  ;; Read from the given stream fd into 'vector', which has element type card8.
  ;; Start storing at index 'start-index' and read exactly 'length' bytes.
  ;; Return t if an error or eof occurred, nil otherwise.
  (declare (fixnum next-index start-index length))
  (with-interrupt-checking-on
      (let ((end-index (+ start-index length)))
	(loop
	  (let ((next-index (excl:read-vector vector fd 
					 :start start-index
					 :end end-index)))
	    (excl:if* (eq next-index start-index)
	       then ; end of file before was all filled up
		    (return t)
	     elseif (eq next-index end-index)
	       then ; we're all done
		    (return nil)
	       else (setq start-index next-index)))))))


#-(or (version>= 5 0) mswindows)
(unless (ff:get-entry-point (ff:convert-to-lang "fd_wait_for_input"))
  (ff:remove-entry-point (ff:convert-to-lang "fd_wait_for_input"))
  #+dlfcn (load "clx:excldep.so")
  #+dlhp  (load "clx:excldep.sl")
  #-dynload (load "clx:excldep.o"))

#-(or (version>= 5 0) mswindows)
(unless (ff:get-entry-point (ff:convert-to-lang "connect_to_server"))
  (ff:remove-entry-point (ff:convert-to-lang "connect_to_server" :language :c))
  #+dlfcn (load "clx:socket.so")
  #+dlhp (load "clx:socket.sl")
  #-dynload (load "clx:socket.o"))

#-(or (version>= 5 0) mswindows)
(ff:defforeign-list `((connect-to-server
		       :entry-point
		       ,(ff:convert-to-lang "connect_to_server")
		       :return-type :fixnum
		       :arg-checking nil
		       :strings-convert nil ; cac 25feb00
		       :arguments (string fixnum))
		      (fd-wait-for-input
		       :entry-point ,(ff:convert-to-lang "fd_wait_for_input")
		       :return-type :fixnum
		       :arg-checking nil
		       :call-direct t
		       :callback nil
		       :allow-other-keys t
		       :arguments (fixnum fixnum))))

#+(and (version>= 5 0) (not mswindows))
(progn
  (unless (excl::get-entry-point (excl::convert-foreign-name "fd_wait_for_input"))
    (load (format nil "clx:excldep.~a" (car excl::*load-foreign-types*))))

  (unless (excl::get-entry-point (excl::convert-foreign-name "connect_to_server"))
    (load (format nil "clx:socket.~a" (car excl::*load-foreign-types*))))

  (ff:def-foreign-call (connect-to-server "connect_to_server")
      ((host (* :char) simple-string) (display :int fixnum))
    :returning (:int fixnum)
    :strings-convert nil		; cac 25feb00
    :arg-checking nil)

  (ff:def-foreign-call (fd-wait-for-input "fd_wait_for_input")
      ((fd :int fixnum) (timeout :int fixnum))
    :returning (:int fixnum)
    :call-direct t
    :arg-checking nil)
)


(eval-when (compile)
  (declaim (declaration buffer-bytes))
  )
