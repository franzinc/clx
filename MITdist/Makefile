# 
#  Makefile for CLX
#  (X11 R4.4 release, Franz Allegro Common Lisp version)
#

# *************************************************************************
# * Change the next line to point to where you have Common Lisp installed *
# *        (make sure the Lisp doesn't already have CLX loaded in)        *
# *************************************************************************
CL	= /usr/local/bin/cl

RM	= /bin/rm
SHELL	= /bin/sh
ECHO	= /bin/echo
TAGS	= /usr/local/lib/emacs/etc/etags

# Name of dumped lisp
CLX	= CLX

CLOPTS	= -qq

# Use this one for Suns
CFLAGS	= -O -DUNIXCONN
# Use this one for Silicon Graphics & Mips Inc MIPS based machines
# CFLAGS = -O -G 0 -I/usr/include/bsd
# Use this one for DEC MIPS based machines
# CFLAGS = -O -G 0 -DUNIXCONN
# Use this one for HP machines
# CFLAGS = -O -DSYSV -DUNIXCONN


# Lisp optimization for compiling
SPEED	= 3
SAFETY	= 0


C_SRC	= excldep.c socket.c
C_OBJS	= excldep.o socket.o

L_OBJS	= defsystem.fasl package.fasl excldep.fasl depdefs.fasl clx.fasl \
	dependent.fasl exclcmac.fasl macros.fasl bufmac.fasl buffer.fasl \
	display.fasl gcontext.fasl requests.fasl input.fasl fonts.fasl \
	graphics.fasl text.fasl attributes.fasl translate.fasl keysyms.fasl \
	manager.fasl image.fasl resource.fasl

L_NOMACROS_OBJS	= package.fasl excldep.fasl depdefs.fasl clx.fasl \
	dependent.fasl buffer.fasl display.fasl gcontext.fasl \
	requests.fasl input.fasl fonts.fasl graphics.fasl text.fasl \
	attributes.fasl translate.fasl keysyms.fasl manager.fasl image.fasl \
	resource.fasl

L_SRC	= defsystem.cl package.cl excldep.cl depdefs.cl clx.cl \
	dependent.cl exclcmac.cl macros.cl bufmac.cl buffer.cl \
	display.cl gcontext.cl requests.cl input.cl fonts.cl \
	graphics.cl text.cl attributes.cl translate.cl keysyms.cl \
	manager.cl image.cl resource.cl

# default and aliases
all:	no-clos
# all:	partial-clos
compile-CLX-for-CLUE:	compile-partial-clos-CLX
clue:	partial-clos

#
# Three build rules are provided: no-clos, partial-clos, and full-clos.
# The first is no-clos, which results in a CLX whose datastructures are
# all defstructs.  partial-clos results in xlib:window, xlib:pixmap, and
# xlib:drawable being CLOS instances, all others defstructs.  full-clos
# makes all CLX complex datatypes into CLOS instances.
#
# (note that the :clos feature implies native CLOS *not* PCL).
#

no-clos:	$(C_OBJS) compile-no-clos-CLX cat

#
# This rule is used to compile CLX to be used with XCW version 2, or CLUE.
#
partial-clos:	$(C_OBJS) compile-partial-clos-CLX cat

full-clos:	$(C_OBJS) compile-full-clos-CLX cat


c:	$(C_OBJS)


compile-no-clos-CLX:	$(C_OBJS)
	$(ECHO) " \
	(proclaim '(optimize (speed $(SPEED)) (safety $(SAFETY)))) \
	#+(version>= 4 0) (pushnew :clx-ansi-common-lisp *features*) \
	(load \"defsystem\") \
	#+allegro (compile-system :clx) \
	#-allegro (compile-clx) \
	#+allegro (compile-system :clx-debug)" \
	| $(CL) $(CLOPTS) -batch

compile-partial-clos-CLX:	$(C_OBJS)
	$(ECHO) " \
	#-clos (setq excl::*print-nickname* t) \
	(proclaim '(optimize (speed $(SPEED)) (safety $(SAFETY)))) \
	(unless (or (find-package 'clos) (find-package 'pcl)) \
	  (let ((spread (sys:gsgc-parameter :generation-spread))) \
	    (setf (sys:gsgc-parameter :generation-spread) 1) \
	    (require :pcl) \
	    (provide :pcl) \
	    (gc) (gc) \
	    (setf (sys:gsgc-parameter :generation-spread) spread))) \
	#+(version>= 4 0) (pushnew :clx-ansi-common-lisp *features*) \
	(load \"defsystem\") \
	(load \"package\") \
	(setq xlib::*def-clx-class-use-defclass* '(xlib:window xlib:pixmap xlib:drawable)) \
	#+allegro (compile-system :clx) \
	#-allegro (compile-clx \"\" \"\" :for-clue t) \
	#+allegro (compile-system :clx-debug)" \
	| $(CL) $(CLOPTS) -batch

compile-full-clos-CLX:	$(C_OBJS)
	$(ECHO) " \
	#-clos (setq excl::*print-nickname* t) \
	(proclaim '(optimize (speed $(SPEED)) (safety $(SAFETY)))) \
	(unless (or (find-package 'clos) (find-package 'pcl)) \
	  (let ((spread (sys:gsgc-parameter :generation-spread))) \
	    (setf (sys:gsgc-parameter :generation-spread) 1) \
	    (require :pcl) \
	    (provide :pcl) \
	    (gc) (gc) \
	    (setf (sys:gsgc-parameter :generation-spread) spread))) \
	#+(version>= 4 0) (pushnew :clx-ansi-common-lisp *features*) \
	(load \"defsystem\") \
	(load \"package\") \
	(setq xlib::*def-clx-class-use-defclass* t) \
	#+allegro (compile-system :clx) \
	#-allegro (compile-clx \"\" \"\" :for-clue t) \
	#+allegro (compile-system :clx-debug)" \
	| $(CL) $(CLOPTS) -batch


cat:
	-cat $(L_NOMACROS_OBJS) > CLX.fasl


load-CLX:
	$(ECHO) " \
	(let ((spread (sys:gsgc-parameter :generation-spread))) \
	  (setf (sys:gsgc-parameter :generation-spread) 1) \
	  (load \"defsystem\") \
	  #+allegro (load-system :clx) \
	  #-allegro (load-clx) \
	  (gc :tenure) \
	  (setf (sys:gsgc-parameter :generation-spread) spread)) \
	(gc t)" \
	'(dumplisp :name "$(CLX)" #+allegro :checkpoint #+allegro nil)' \
	"(exit)" | $(CL) $(CLOPTS)

clean:
	$(RM) -f *.fasl debug/*.fasl $(CLX) core $(C_OBJS) make.out


install:
	mv CLX.fasl $(DEST)/clx.fasl
	mv *.o $(DEST)


tags:
	$(TAGS) $(L_SRC) $(C_SRC)
