# $Id: Makefile,v 1.13 1997/10/20 22:51:39 layer Exp $
#  Makefile for CLX

# For versions prior to ACL 5.0 (and 4.3.2 on Windows), comment out the
# following line:
SAVEIMG = yes

# *************************************************************************
# * Change the next line to point to where you have Common Lisp installed *
# *        (make sure the Lisp doesn't already have CLX loaded in)        *
# *************************************************************************
ifdef SAVEIMG
CL		= ../src/lisp -I ../src/dcl.dxl
# Name of dumped lisp
CLX		= clx.dxl
DUMPLISP_ARGS	=
else
CL		= ../src/dcl
# Name of dumped lisp
CLX		= clx
DUMPLISP_ARGS	= :checkpoint nil
endif

SHELL	= /bin/sh
ECHO	= /bin/echo
MV	= /usr/fi/mv-nfs -c
TAGS	= /usr/local/lib/emacs/etc/etags

CLOPTS	= -qq

# Use this one for Silicon Graphics & Mips Inc MIPS based machines
#   XCFLAGS = -G 0 -I/usr/include/bsd
# Use this one for DEC MIPS based machines
#   XCFLAGS = -G 0
# Use this one for HP machines
#   XCFLAGS = -DSYSV

CFLAGS	= -O -DUNIXCONN $(XCFLAGS)

# compile options:
SPEED			= 3
SAFETY			= 0
DEBUG			= 1
RECORD_XREF_INFO	= nil
RECORD_SOURCE_FILE_INFO	= nil
SAVE_LOCAL_NAMES	= nil
LOAD_XREF_INFO		= nil
LOAD_SOURCE_FILE_INFO	= nil
LOAD_LOCAL_NAMES_INFO	= nil
gc_print		= nil
compile_verbose		= t
compile_print		= nil
redef_warning		= t

C_SRC	= excldep.c socket.c
C_OBJS	= excldep.o socket.o
C_SOBJS	= excldep.so socket.so

L_OBJS	= defsystem.fasl package.fasl excldep.fasl depdefs.fasl clx.fasl \
	dependent.fasl exclcmac.fasl macros.fasl bufmac.fasl buffer.fasl \
	display.fasl gcontext.fasl requests.fasl input.fasl fonts.fasl \
	graphics.fasl text.fasl attributes.fasl translate.fasl keysyms.fasl \
	manager.fasl image.fasl resource.fasl

L_NOMACROS_OBJS	= package.fasl excldep.fasl depdefs.fasl clx.fasl \
	dependent.fasl exclcmac.fasl buffer.fasl display.fasl gcontext.fasl \
	requests.fasl input.fasl fonts.fasl graphics.fasl text.fasl \
	attributes.fasl translate.fasl keysyms.fasl manager.fasl image.fasl \
	resource.fasl

L_SRC	= defsystem.cl package.cl excldep.cl depdefs.cl clx.cl \
	dependent.cl exclcmac.cl macros.cl bufmac.cl buffer.cl \
	display.cl gcontext.cl requests.cl input.cl fonts.cl \
	graphics.cl text.cl attributes.cl translate.cl keysyms.cl \
	manager.cl image.cl resource.cl

PICFLAGS = -K pic
SHAREFLAGS = 
MAKE_SHARED = ld -G
IMPORTS =

# default and aliases
# all:	no-clos
all:	partial-clos
compile-CLX-for-CLUE:	compile-partial-clos-CLX
clue:	partial-clos

excldep.so: excldep.c
	-mv excldep.o excldep.ooo
	cc $(CFLAGS) -c $(PICFLAGS) excldep.c
	$(MAKE_SHARED) $(SHAREFLAGS) -o excldep.so excldep.o $(IMPORTS)
	rm -f excldep.o
	-mv excldep.ooo excldep.o

excldep.sl: excldep.c
	-mv excldep.o excldep.ooo
	cc $(CFLAGS) -c $(PICFLAGS) excldep.c
	$(MAKE_SHARED) $(SHAREFLAGS) -o excldep.sl excldep.o $(IMPORTS)
	rm -f excldep.o
	-mv excldep.ooo excldep.o

socket.so: socket.c
	-mv socket.o socket.ooo
	cc $(CFLAGS) -c $(PICFLAGS) socket.c
	$(MAKE_SHARED) $(SHAREFLAGS) -o socket.so socket.o $(IMPORTS)
	rm -f socket.o
	-mv socket.ooo socket.o

socket.sl: socket.c
	-mv socket.o socket.ooo
	cc $(CFLAGS) -c $(PICFLAGS) socket.c
	$(MAKE_SHARED) $(SHAREFLAGS) -o socket.sl socket.o $(IMPORTS)
	rm -f socket.o
	-mv socket.ooo socket.o

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
	$(ECHO) "(pushnew :clx-ansi-common-lisp *features*) \
	(load-logical-pathname-translations \"clx\") \
	(load \"defsystem\") \
	(proclaim '(optimize \
			(safety $(SAFETY)) \
			(speed $(SPEED)) \
			(debug $(DEBUG)))) \
	(let ((*record-source-file-info* $(RECORD_SOURCE_FILE_INFO)) \
	      (*load-source-file-info* $(LOAD_SOURCE_FILE_INFO)) \
	      (*load-local-names-info* $(LOAD_LOCAL_NAMES_INFO)) \
	      (*record-xref-info* $(RECORD_XREF_INFO)) \
	      (*load-xref-info* $(LOAD_XREF_INFO)) \
	      (comp::save-local-names-switch $(SAVE_LOCAL_NAMES)) \
	      (*compile-print* $(compile_print)) \
	      (*compile-verbose* $(compile_verbose))) \
	    (setf (sys:gsgc-switch :print) $(gc_print)) \
	    (compile-system :clx) \
	    (compile-system :clx-debug))" | $(CL) $(CLOPTS) -batch

compile-partial-clos-CLX:	$(C_OBJS)
	$(ECHO) "#-mswindows (ff:get-entry-point (ff:convert-to-lang \"fd_wait_for_input\")) \
	(pushnew :clx-ansi-common-lisp *features*) \
	(load-logical-pathname-translations \"clx\") \
	(load \"defsystem\") \
	(load \"package\") \
	(setq xlib::*def-clx-class-use-defclass* \
	  '(xlib:window xlib:pixmap xlib:drawable)) \
	(proclaim '(optimize \
			(safety $(SAFETY)) \
			(speed $(SPEED)) \
			(debug $(DEBUG)))) \
	(let ((*record-source-file-info* $(RECORD_SOURCE_FILE_INFO)) \
	      (*load-source-file-info* $(LOAD_SOURCE_FILE_INFO)) \
	      (*load-local-names-info* $(LOAD_LOCAL_NAMES_INFO)) \
	      (*record-xref-info* $(RECORD_XREF_INFO)) \
	      (*load-xref-info* $(LOAD_XREF_INFO)) \
	      (comp::save-local-names-switch $(SAVE_LOCAL_NAMES)) \
	      (*compile-verbose* $(compile_verbose)) \
	      (*compile-print* $(compile_print))) \
	    (setf (sys:gsgc-switch :print) $(gc_print)) \
	    (compile-system :clx) \
	    (compile-system :clx-debug))" | $(CL) $(CLOPTS) -batch -backtrace-on-error

compile-full-clos-CLX:	$(C_OBJS)
	$(ECHO) "(pushnew :clx-ansi-common-lisp *features*) \
	(load-logical-pathname-translations \"clx\") \
	(load \"defsystem\") \
	(load \"package\") \
	(setq xlib::*def-clx-class-use-defclass* t) \
	(proclaim '(optimize \
			(safety $(SAFETY)) \
			(speed $(SPEED)) \
			(debug $(DEBUG)))) \
	(let ((*record-source-file-info* $(RECORD_SOURCE_FILE_INFO)) \
	      (*load-source-file-info* $(LOAD_SOURCE_FILE_INFO)) \
	      (*load-local-names-info* $(LOAD_LOCAL_NAMES_INFO)) \
	      (*record-xref-info* $(RECORD_XREF_INFO)) \
	      (*load-xref-info* $(LOAD_XREF_INFO)) \
	      (comp::save-local-names-switch $(SAVE_LOCAL_NAMES)) \
	      (*compile-print* $(compile_print)) \
	      (*compile-verbose* $(compile_verbose))) \
	    (setf (sys:gsgc-switch :print) $(gc_print)) \
	    (compile-system :clx) \
	    (compile-system :clx-debug))" | $(CL) $(CLOPTS) -batch

cat:
	-cat $(L_NOMACROS_OBJS) > bigclx.fasl

load-CLX:
	$(ECHO) "\
	(load-logical-pathname-translations \"clx\") \
	(load-application \
	  (progn (load \"defsystem\") (load-system :clx)) \
	  :global-gc t :devel nil)" \
	'(dumplisp :name "$(CLX)" $(DUMPLISP_ARGS))' \
	"(exit)" | $(CL) $(CLOPTS)

clean_OS:
	rm -f *.o *.so *.sl

clean:
	rm -f *.fasl *.o *.so *.sl debug/*.fasl $(CLX) core make.out

install_OS:
	$(MV) *.o $(DEST)
	if test -f socket.so; then \
		$(MV) *.so $(DEST); \
	fi
	if test -f socket.sl; then \
		$(MV) *.sl $(DEST); \
	fi

install: install_OS
	$(MV) bigclx.fasl $(DEST)/clx.fasl

tags:
	$(TAGS) $(L_SRC) $(C_SRC)

DISTFILES = CHANGES ChangeLog Makefile */Makefile NEWCHANGES README */README \
	*.c *.cl */*.cl doc/*.Z

echo-distribution-source-files:
	@echo $(DISTFILES)
