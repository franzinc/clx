;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- exclcmac.cl
;;;           This file provides for inline expansion of some functions.
;;;
;;; Copyright (c) 1989 Franz Inc, Berkeley, Ca.
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


(defconstant type-pred-alist
    '(#-(version>= 4 1 devel 16)
      (card8  . card8p)
      #-(version>= 4 1 devel 16)
      (card16 . card16p)
      #-(version>= 4 1 devel 16)
      (card29 . card29p)
      #-(version>= 4 1 devel 16)
      (card32 . card32p)
      #-(version>= 4 1 devel 16)
      (int8   . int8p)
      #-(version>= 4 1 devel 16)
      (int16  . int16p)
      #-(version>= 4 1 devel 16)
      (int32  . int32p)
      #-(version>= 4 1 devel 16)
      (mask16 . card16p)
      #-(version>= 4 1 devel 16)
      (mask32 . card32p)
      #-(version>= 4 1 devel 16)
      (pixel  . card32p)
      #-(version>= 4 1 devel 16)
      (resource-id . card29p)
      #-(version>= 4 1 devel 16)
      (keysym . card32p)
      (angle  . anglep)
      (color  . color-p)
      (bitmap-format . bitmap-format-p)
      (pixmap-format . pixmap-format-p)
      (display  . display-p)
      (drawable . drawable-p)
      (window   . window-p)
      (pixmap   . pixmap-p)
      (visual-info . visual-info-p)
      (colormap . colormap-p)
      (cursor . cursor-p)
      (gcontext .  gcontext-p)
      (screen . screen-p)
      (font . font-p)
      #|
;; These types don't exist when this file gets loaded.  Perhaps the def-clx-class
;; forms could be moved from image.cl and manager.cl to clx.cl.
      (image-x . image-x-p)
      (image-xy . image-xy-p)
      (image-z . image-z-p)
      (wm-hints . wm-hints-p)
      (wm-size-hints . wm-size-hints-p)
|#
      ))

;; This (if (and ...) t nil) stuff has a purpose -- it lets the old
;; sun4 compiler opencode the `and'.

#-(version>= 4 1 devel 16)
(defun card8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 8) x) (>= x 0))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun card16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 16) x) (>= x 0))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun card29p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) 0))
	  (and (excl:bignump x) (> #.(expt 2 29) (the bignum x))
	       (>= (the bignum x) 0)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun card32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) 0))
	  (and (excl:bignump x) (> #.(expt 2 32) (the bignum x))
	       (>= (the bignum x) 0)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun int8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 7) x) (>= x #.(expt -2 7)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun int16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 15) x) (>= x #.(expt -2 15)))
      t
    nil))

#-(version>= 4 1 devel 16)
(defun int32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (excl:fixnump x)
	  (and (excl:bignump x) (> #.(expt 2 31) (the bignum x))
	       (>= (the bignum x) #.(expt -2 31))))
      t
    nil))

;; This one can be handled better by knowing a little about what we're
;; testing for.  Plus this version can handle (single-float pi), which
;; is otherwise larger than pi!
(defun anglep (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl::fixnump x) (>= (the fixnum x) #.(truncate (* -2 pi)))
	       (<= (the fixnum x) #.(truncate (* 2 pi))))
	  (and (excl::single-float-p x)
	       (>= (the single-float x) #.(float (* -2 pi) 0.0s0))
	       (<= (the single-float x) #.(float (* 2 pi) 0.0s0)))
	  (and (excl::double-float-p x)
	       (>= (the double-float x) #.(float (* -2 pi) 0.0d0))
	       (<= (the double-float x) #.(float (* 2 pi) 0.0d0))))
      t
    nil))

#-(version>= 4 1 devel 16)
(define-compiler-macro card8p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 8) ,xx) (>= ,xx 0)))))

#-(version>= 4 1 devel 16)
(define-compiler-macro card16p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 16) ,xx) (>= ,xx 0)))))

#-(version>= 4 1 devel 16)
(define-compiler-macro int8p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 7) ,xx) (>= ,xx #.(expt -2 7))))))

#-(version>= 4 1 devel 16)
(define-compiler-macro int16p (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       (declare (optimize (speed 3) (safety 0))
		(fixnum ,xx))
       (and (excl:fixnump ,xx) (> #.(expt 2 15) ,xx) (>= ,xx #.(expt -2 15))))))

;; Card29p, card32p, int32p are too large to expand inline.

(eval-when (load eval)
  #+(version>= 4 1 devel 16)
  (mapcar #'(lambda (elt) (excl:add-typep-transformer (car elt) (cdr elt)))
	  type-pred-alist)
  #-(version>= 4 1 devel 16)
  (nconc excl::type-pred-alist type-pred-alist))


;;
;; Type transformers
;;
(define-compiler-macro card8->int8 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card8 ,xx))
       (the int8 (if (logbitp 7 ,xx)
		     (the int8 (- ,xx #x100))
		   ,xx)))))
(define-compiler-macro int8->card8 (x)
  `(locally ,(declare-bufmac)
     (the card8 (ldb (byte 8 0) (the int8 ,x)))))

(define-compiler-macro card16->int16 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card16 ,xx))
       (the int16 (if (logbitp 15 ,xx)
		      (the int16 (- ,xx #x10000))
		    ,xx)))))

(define-compiler-macro int16->card16 (x)
  `(locally ,(declare-bufmac)
     (the card16 (ldb (byte 16 0) (the int16 ,x)))))

(define-compiler-macro card32->int32 (x)
  (let ((xx (gensym)))
    `(let ((,xx ,x))
       ,(declare-bufmac)
       (declare (type card32 ,xx))
       (the int32 (if (logbitp 31 ,xx)
		      (the int32 (- ,xx #x100000000))
		    ,xx)))))

(define-compiler-macro int32->card32 (x)
  `(locally ,(declare-bufmac)
     (the card32 (ldb (byte 32 0) (the int32 ,x)))))

(define-compiler-macro char->card8 (char)
  `(locally ,(declare-bufmac)
     (the card8 (char-code (the excl::string-char ,char)))))

(define-compiler-macro card8->char (card8)
  `(locally ,(declare-bufmac)
     (the excl::string-char (code-char (the card8 ,card8)))))


;;
;; Array accessors and setters
;;
(define-compiler-macro aref-card8 (a i)
  `(locally ,(declare-bufmac)
     (the card8 (sys:memref (the buffer-bytes ,a)
			    #.(sys::mdparam 'comp::md-svector-data0-norm)
			    (the array-index ,i)
			    :unsigned-byte))))

(define-compiler-macro aset-card8 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :unsigned-byte)
	   (the card8 ,v))))

(define-compiler-macro aref-int8 (a i)
  `(locally ,(declare-bufmac)
     (the int8 (sys:memref (the buffer-bytes ,a)
			   #.(sys::mdparam 'comp::md-svector-data0-norm)
			   (the array-index ,i)
			   :signed-byte))))

(define-compiler-macro aset-int8 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :signed-byte)
       (the int8 ,v))))

(define-compiler-macro aref-card16 (a i)
  `(locally ,(declare-bufmac)
     (the card16 (sys:memref (the buffer-bytes ,a)
			     #.(sys::mdparam 'comp::md-svector-data0-norm)
			     (the array-index ,i)
			     :unsigned-word))))

(define-compiler-macro aset-card16 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :unsigned-word)
	   (the card16 ,v))))

(define-compiler-macro aref-int16 (a i)
  `(locally ,(declare-bufmac)
     (the int16 (sys:memref (the buffer-bytes ,a)
			    #.(sys::mdparam 'comp::md-svector-data0-norm)
			    (the array-index ,i)
			    :signed-word))))

(define-compiler-macro aset-int16 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :signed-word)
       (the int16 ,v))))

(define-compiler-macro aref-card32 (a i)
  `(locally ,(declare-bufmac)
     (the card32 (sys:memref (the buffer-bytes ,a)
			     #.(sys::mdparam 'comp::md-svector-data0-norm)
			     (the array-index ,i)
			     :unsigned-long32))))

(define-compiler-macro aset-card32 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :unsigned-long32)
       (the card32 ,v))))

(define-compiler-macro aref-int32 (a i)
  `(locally ,(declare-bufmac)
     (the int32 (sys:memref (the buffer-bytes ,a)
			    #.(sys::mdparam 'comp::md-svector-data0-norm)
			    (the array-index ,i)
			    :signed-long))))

(define-compiler-macro aset-int32 (v a i)
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :signed-long)
       (the int32 ,v))))

(define-compiler-macro aref-card29 (a i)
  ;; Don't need to mask bits here since X protocol guarantees top bits zero
  `(locally ,(declare-bufmac)
     (the card29 (sys:memref (the buffer-bytes ,a)
			     #.(sys::mdparam 'comp::md-svector-data0-norm)
			     (the array-index ,i)
			     :unsigned-long32))))

(define-compiler-macro aset-card29 (v a i)
  ;; I also assume here Lisp is passing a number that fits in 29 bits.
  `(locally ,(declare-bufmac)
     (setf (sys:memref (the buffer-bytes ,a)
		       #.(sys::mdparam 'comp::md-svector-data0-norm)
		       (the array-index ,i)
		       :unsigned-long32)
       (the card29 ,v))))

;;
;; Font accessors
;;
(define-compiler-macro font-id (font)
  ;; Get font-id, opening font if needed
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-id-internal ,f)
	   (open-font-internal ,f)))))

(define-compiler-macro font-font-info (font)
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-font-info-internal ,f)
	   (query-font ,f)))))

(define-compiler-macro font-char-infos (font)
  (let ((f (gensym)))
    `(let ((,f ,font))
       (or (font-char-infos-internal ,f)
	   (progn (query-font ,f)
		  (font-char-infos-internal ,f))))))


;;
;; Miscellaneous
;;
(define-compiler-macro current-process ()
  `(the (or mp::process null) (and #-process7 mp::*scheduler-stack-group*
				   #+process7 (si:scheduler-running-p)
				  mp::*current-process*)))

(define-compiler-macro process-wakeup (process)
  (let ((proc (gensym)))
    `(let ((.pw-curproc. mp::*current-process*)
	   (,proc ,process))
       (when (and .pw-curproc. ,proc)
	 (if (> (mp::process-priority ,proc)
		(mp::process-priority .pw-curproc.))
	     (mp::process-allow-schedule ,proc))))))

(define-compiler-macro buffer-new-request-number (buffer)
  (let ((buf (gensym)))
    `(let ((,buf ,buffer))
       (declare (type buffer ,buf))
       (setf (buffer-request-number ,buf)
	 (ldb (byte 16 0) (1+ (buffer-request-number ,buf)))))))
