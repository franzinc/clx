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
#-(and (version>= 6) clx-use-allegro-streams)
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

#+(and (version>= 6) clx-use-allegro-streams)
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

  (ff:def-foreign-call (fd-wait-for-input "fd_wait_for_input")
      ((fd :int fixnum) (timeout :int fixnum))
    :returning (:int fixnum)
    :call-direct t
    :arg-checking nil)
  )

#+(and (version>= 5 0) (not clx-use-allegro-streams))
(progn
  (unless (excl::get-entry-point (excl::convert-foreign-name "connect_to_server"))
    (load (format nil "clx:socket.~a" (car excl::*load-foreign-types*))))

  (ff:def-foreign-call (connect-to-server "connect_to_server")
      ((host (* :char) simple-string) (display :int fixnum))
    :returning (:int fixnum)
    :strings-convert nil		; cac 25feb00
    :arg-checking nil)
  )


(eval-when (compile)
  (declaim (declaration buffer-bytes))
  )

;; Bring in and rename the old filesys-read-bytes from where it is
;; defined in streamc.cl, but streamc might no longer be available:

(in-package :excl)

(eval-when (compile eval)
  (require :iodefs))

(defun excl-write-bytes (handle svector start-index length stream)
  ;; Write the contents of the simple-vector to the given handle.
  ;; Start at start-index and write length bytes, counting 8 bit bytes.
  ;; The loop and retry on EINTR is necessary on some machines because random
  ;; alarm signals can cause a write that has been blocked to return EINTR.
  (declare (optimize speed)
	   (type adim start-index length))
  (let ((*stream-for-sigpipe* (or *stream-for-sigpipe* stream)))
    (fast
     (loop
       (multiple-value-bind (written errcode)
	   (if* (streamp handle)
	      then (let ((res (write-vector-2 svector handle start-index (+ start-index length) 0 0)))
		     (declare (type adim res))
		     (- res start-index))
	      else (excl::.primcall 'sys::filesys #.fs-write-bytes
				    handle svector start-index length))
	 (if* errcode
	    then (if* (or (eq errcode (fast *error-code-would-block*))
			  (eq errcode (fast *error-code-eagain*)))
		    then (let* ((fnn (- -1 handle))
				(sys::*stack-group-watchfor-fds*
				 (cons fnn sys::*stack-group-watchfor-fds*)))
			   (excl::funcall-in-package
			    :process-wait :multiprocessing "Blocked on output to socket"
			    #'write-no-hang-p handle)) ;; [bug11901], [bug12195]
		  elseif (not (eq errcode *error-code-interrupted-system-call*))
		    then (.errno-stream-error "writing bytes to" handle errcode))
		 (setq written 0)	; if error assume 0 written
	    else (decf length written)
		 (if* (<= length 0)
		    then (return t)
		    else (incf start-index written))))))))
