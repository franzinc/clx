;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- excldep.cl
;;;
;;; copyright (c) 1987-1989 Franz Inc, Berkeley, CA - All rights reserved.
;; copyright (c) 1989-2004 Franz Inc, Oakland, CA - All rights reserved.
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
  (declare (fixnum start-index length))
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
  (let ( ;; [bug16880]:
	#+ignore (*stream-for-sigpipe* (or *stream-for-sigpipe* stream)))
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
				(sys::*thread-watchfor-fds*
				 (cons fnn sys::*thread-watchfor-fds*))
				)
			   (excl::funcall-in-package
			    :process-wait :multiprocessing "Blocked on output to socket"
			    #'write-no-hang-p handle)) ;; [bug11901], [bug12195]
		  elseif (eql errcode
			      #-mswindows 32 ; sigpipe
			      #+mswindows 10053 ; WSAECONNABORTED
			      )
		    then ;; [bug16880]: indicate that this stream can't be written to any more
			 (xlib::close-buffer stream)
		  elseif (not (eq errcode *error-code-interrupted-system-call*))
		    then (.errno-stream-error "writing bytes to" handle errcode))
		 (setq written 0)	; if error assume 0 written
	    else (decf length written)
		 (if* (<= length 0)
		    then (return t)
		    else (incf start-index written))))))))



#+(version>= 8 2)
(in-package :excl)

#+(version>= 8 2)
(eval-when (compile load eval)
  (pushnew :allegro-pre-smp *features*)
  (or (fboundp 'excl::with-strong-spin-consistent-bindings)
      (pushnew :strong-spin-lock *features*)))

#+strong-spin-lock
(eval-when (compile load eval)
(defpackage :excl
  (:export
   ;; Use spin-lock to serialize readers and writers.
   ;; Garanteed bounded delay to any writer or reader.
   #:with-strong-spin-consistent-bindings
   #:with-strong-consistency-spin-lock
   ))

;;; When #+smp-coarse-monitor generate code that counts total number
;;;  times an atomic operation had to branch back to try again.
;;; Count is kept in *smp-loop-count* and *smp-loop-back-count*.
(defvar *smp-loop-count* 0)
(defvar *smp-loop-back-count* 0)

;;; When #+smp-detailed-monitor generate code that counts the number of
;;;  times each smp atomic update loop had to branch back to try again.
;;; Counts are kept in vectors *smp-loop-counts* and *smp-loop-back-counts*.
;;; Index to counts vectors is in *smp-loop-names*.
(defvar *smp-loop-index* 0)
(defvar *smp-loop-counts* (make-array 1000 :initial-element 0))
(defvar *smp-loop-back-counts* (make-array 1000 :initial-element 0))
(defvar *smp-loop-names* nil)


(defmacro smploop (name sub-name &body body &aux index newlen)
  "Generate a loop with debug counter on branch back"
  (declare (ignorable index newlen))

  (when (and (member :smp-coarse-monitor *features* :test #'string-equal)
	     (member :smp-detailed-monitor *features* :test #'string-equal))
    (error "Conflicting features: smp-coarse-monitor smp-detailed-monitor"))

  (or (and sub-name (symbolp sub-name))
      (error "Sub-name must be a non-nil symbol: ~S ~S" name sub-name))
  (setf name (intern (format nil "~A-~A" (or name (gensym)) sub-name)))

  (when (member :smp-detailed-monitor *features* :test #'string-equal)
    (let ((place (assoc name *smp-loop-names*)))
      (if place
	  (setf index (cdr place))
	(push (setf place (cons name (setf index *smp-loop-index*)))
	      *smp-loop-names*))
      (incf *smp-loop-index*)
      (when (not (< *smp-loop-index* (length *smp-loop-counts*)))
	(setf newlen (+ 1000 (length *smp-loop-back-counts*)))
	(setf *smp-loop-counts* (make-array newlen :initial-element 0))
	(setf *smp-loop-back-counts* (make-array newlen :initial-element 0))
	)))
  `(loop

    ,@(when (member :smp-coarse-monitor *features* :test #'string-equal)
	`((incf (the fixnum *smp-loop-count*))))

    ,@(when (member :smp-detailed-monitor *features* :test #'string-equal)
	`((incf (the fixnum (svref (the simple-vector *smp-loop-counts*)
			     (the fixnum ,index))))))

    ,@body
    
    ,@(when (member :smp-coarse-monitor *features* :test #'string-equal)
	`((incf (the fixnum *smp-loop-back-count*))))

    ,@(when (member :smp-detailed-monitor *features* :test #'string-equal)
	`((incf (the fixnum (svref (the simple-vector *smp-loop-back-counts*)
			     (the fixnum ,index))))))

    ))
    
(defmacro with-strong-spin-consistent-bindings ((&rest bindings)
					 (control-place ;; incf-atomic place
					  ;; room for more keyword args
					  &key
					  (max-readers 20)
					  name
					  )
					 &body body &environment env)
  " Evaluate body with vars bound to a consistent set of values.
It is assumed that the body code does not modify the value of
control-place, or any of the locations used to bind the variables.
It is assumed that modifications are made inside the body of a
with-consistency-spin-lock expressionand 
modifications will change the value of control-place atomically.

The variables are bound in a let form.

The value(s) returned are the value(s) returned by the last
form in the body.  This macro generates a loop context;  therefore,
an unnamed return will cause unexpected behavior.

The control-place argument must be a place suitable for
incf-atomic.

The max-readers argument specifies the maximum number of simultaneous readers
allowed at any time.  This number is the upper bound on the delay a writer can suffer.
The default is 20.  If a value is specified, it must be a number visible at 
compile time.

This macro generates more light-weight code than the with-lock-consistent-bindings macro,
but with a more limited behavior.  If there are many more threads than processors,
then the spin loops may take a whole scheduling quantum to recover from a 
collision.  If any reader threads are running, a writing thread will spin until
the readers are done.  If a writer thread is running, any reader threads will 
spin until the writing is done.  Both the reader and the writer bodies should complete
in a short time since all the bodies will run sequentially.

A writer thread
will only be delayed by readers that were active at the moment the writer inidicated
a need to write.  
"
  (multiple-value-bind (subform-vars subform-forms
				     oldval-var newval-var
				     cw-form r-form)
      (get-atomic-modify-expansion control-place env nil)
    (flet ((tvarname (b)
		     (gensym (symbol-name (if (consp b) (car b) b))))
	   (tvar-setq (tv b)
		      `(setq ,tv ,(and (consp b) (cadr b))))
	   (bind-from-tvar (tv b)
			   `(,(if (consp b) (car b) b) ,tv)))
      (let ((tvars (mapcar #'tvarname bindings)))

	;; MORE STRICT SPIN CONSISTENCY 
	;; 
	;;  control-place-content -> r...rwp
	;;    r...r -> high-order bits can be 0...0 or 
	;;                 b...b where at least one of the b bits is 1
	;;  Reader logic:
	;;      spin until we can update to s...sx0
	;;           where s...s is r...r + 1 and x is 0 or 1
	;;           AND r...r < max-readers
	;;      spin until w-bit is zero
	;;      read values
	;;      r-1 atomically
	;;  Writer logic:
	;;      update wp from 00 to 01 atomically
	;;      spin until r...rwp is 0...001
	;;      set r...rwp to 0...010 atomically
	;;      make changes
	;;      decrement atomically r...rwp by 2 to set wp to 0p
	;;              p may have been set to 1 by another writer
	;;              if r...r bits are non zero, readers are spinning
	;;              if p is 1, the next writer will spin until
	;;                   the new batch of readers is done, then run
	;;  Once a writer has signalled the need to write, new readers
	;;   will not start, so writer is delayed only by the fixed
	;;   number of readers active at the moment the writer is entered.
	;;  Multiple writer threads cannot block readers indefinitely
	;;   either.  Pending readers get a chance to run between
	;;   writer slices.


	`(let (,oldval-var
	       ,newval-var
	       ,@(mapcar 'list subform-vars subform-forms)
	       ,@tvars)
	   (declare (fixnum ,oldval-var ,newval-var)) 
	   (smploop
	    ,name strong-inc
	    ;; Spin until we can update the reader count.
	    ;;   Reader count can be incremented only whne there
	    ;;   are no pending writers.
	    (setq ,oldval-var 
		  (logand #xfffffc ,r-form) ;;; zero out wp bits
		  ,newval-var 
		  (+ 4 ,oldval-var) ;;; increment r...r bits to s...s
		  )
	    (when (< ,oldval-var ,(* 4 max-readers))
	      ;; First, try to set from r...r00 to s...s00
	      (when ,cw-form (return))
	      ;; Then, try to set from r...r10 to s...s10
	      ;; since we do not care about the w bit at this point.
	      (setq ,oldval-var  (+ 2 ,oldval-var)
		    ,newval-var  (+ 2 ,newval-var))
	      (when ,cw-form (return))))
	   (unwind-protect
	       (progn
		 (smploop
		  ,name strong-write-done
		  ;; Spin until a writer, if any, is done.
		  ;; At this point r...r bits, incremented to s...s,
		  ;; may now be incremented or decremented further 
		  ;; to t...t
		  (setq ,oldval-var 
			(logand #xfffffc ,r-form) ;;; zero out wp bits
			,newval-var ,oldval-var)
		  ;; First, try to set from t...t00 to t...t00
		  (when ,cw-form (return))
		  ;; Then, try to set from t...t01 to t...t01
		  ;; since we do not care about the p bit at this point.
		  (setq ,oldval-var  (+ 1 ,oldval-var)
			,newval-var  (+ 1 ,newval-var))
		  (when ,cw-form (return)))
		 ;; Grab our bindings
		 ,@(mapcar #'tvar-setq tvars bindings))
	     ;; uwp cleanup: release our hold on the controlled source
	     ;;   r-1 atomically
	     (smploop
	      ,name strong-dec
	      (setq ,oldval-var ,r-form
		    ,newval-var (- ,oldval-var 4))
	      (when ,cw-form
		(return))))
	   (let ,(mapcar #'bind-from-tvar tvars bindings)
	     ,@body))))))

(defmacro with-strong-consistency-spin-lock ((control-place &key name) 
					     &rest body &environment env)
  "This macro must enclose any modifications to data that is used in the
bindings of a with-strong-spin-consistent-bindings form.

The value(s) returned are the value(s) returned by the last
form in the body.
"
  (multiple-value-bind (subform-vars subform-forms
				     oldval-var newval-var
				     cw-form r-form)
      (get-atomic-modify-expansion control-place env nil)
    `(let (,oldval-var ,newval-var
		       ,@(mapcar 'list subform-vars subform-forms))
       (declare (fixnum ,oldval-var ,newval-var))
	;; MORE STRICT SPIN CONSISTENCY 
	;; 
	;;  control-place-content -> r...rwp
	;;    r...r -> high-order bits can be 0...0 or 
	;;                 b...b where at least one of the b bits is 1
	;;  Reader logic:
	;;      spin until we can update to s...sx0
	;;           where s...s is r...r + 1
	;;      spin until w-bit is zero
	;;      read values
	;;      r-1 atomically
	;;  Writer logic:
	;;      update wp from 00 to 01 atomically
	;;      spin until r...rwp is 0...001
	;;      r...rwp set to 0...010 atomically
	;;      make changes
	;;      decrement atomically r...rwp by 2 to set wp to 0p
	;;              p may have been set to 1 by another writer
	;;              if r...r bits are non zero, readers are spinning
	;;              if p is 1, the next writer will spin until
	;;                   the new batch of readers is done, then run
	;;  Once a writer has signalled the need to write, new readers
	;;   will not start, so writer is delayed only by the fixed
	;;   number of readers active at the moment the writer is entered.
	;;  Multiple writer threads cannot block readers indefinitely
	;;   either.  Pending readers get a chance to run between
	;;   writer slices.


       ;; get a lock on the control
       (smploop
	,name strong-write-start
	;; update wp from 00 to 01 atomically
	(setq ,oldval-var ,r-form
	      ,newval-var (1+ (logand #xfffffc ,oldval-var)))
	(when ,cw-form  (return)))
       ;; spin until r...rwp is 0...001
       ;; r...rwp set to 0...010 atomically
       (setq ,oldval-var 1 ,newval-var 2)
       (smploop 
	,name strong-write-sync
	(when ,cw-form (return)))
       (unwind-protect
	   (progn ,@body)
	 ;; Release the lock no matter what.
	 (smploop
	  ,name strong-write-end
	  ;; decrement atomically r...rwp by 2 to set wp to 0p
	  (setq ,oldval-var ,r-form
		,newval-var (- ,oldval-var 2))
	  (when ,cw-form
	    (return)))))))
)




