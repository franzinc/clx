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

(eval-when (compile load eval)
  #-mswindows (require :foreign)
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

(defmacro with-interrupt-checking-on (&body body)
  `(locally (declare (optimize (safety 1)))
     ,@body))

;; Read from the given fd into 'vector', which has element type card8.
;; Start storing at index 'start-index' and read exactly 'length' bytes.
;; Return t if an error or eof occurred, nil otherwise.
(defun fd-read-bytes (fd vector start-index length)
  (declare (fixnum #-mswindows fd start-index length)
	   (type (simple-array (unsigned-byte 8) (*)) vector))
  (with-interrupt-checking-on
   (do ((rest length))
       ((eq 0 rest) nil)
     (declare (fixnum rest))
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

#-mswindows
(unless (ff:get-entry-point (ff:convert-to-lang "fd_wait_for_input"))
  (ff:remove-entry-point (ff:convert-to-lang "fd_wait_for_input"))
  #+dlfcn (load "clx:excldep.so")
  #+dlhp  (load "clx:excldep.sl")
  #-dynload (load "clx:excldep.o"))

#-mswindows
(unless (ff:get-entry-point (ff:convert-to-lang "connect_to_server"))
  (ff:remove-entry-point (ff:convert-to-lang "connect_to_server" :language :c))
  #+dlfcn (load "clx:socket.so")
  #+dlhp (load "clx:socket.sl")
  #-dynload (load "clx:socket.o"))

#-mswindows
(ff:defforeign-list `((connect-to-server
		       :entry-point
		       ,(ff:convert-to-lang "connect_to_server")
		       :return-type :fixnum
		       :arg-checking nil
		       :arguments (string fixnum))
		      (fd-wait-for-input
		       :entry-point ,(ff:convert-to-lang "fd_wait_for_input")
		       :return-type :fixnum
		       :arg-checking nil
		       :call-direct t
		       :callback nil
		       :allow-other-keys t
		       :arguments (fixnum fixnum))))

(eval-when (compile)
  (declaim (declaration buffer-bytes))
  )
