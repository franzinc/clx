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
CFLAGS	= -O -DUNIXCONN $(XCFLAGS)
# Use this one for Silicon Graphics & Mips Inc MIPS based machines
# CFLAGS = -O -G 0 -I/usr/include/bsd
# Use this one for DEC MIPS based machines
# CFLAGS = -O -G 0 -DUNIXCONN
# Use this one for HP machines
# CFLAGS = -O -DSYSV -DUNIXCONN


# Lisp optimization for compiling
SPEED	= 3
SAFETY	= 0
DEBUG	= 1
RECORD_XREF_INFO = nil
RECORD_SOURCE_FILE_INFO = nil
LOAD_XREF_INFO = nil
LOAD_SOURCE_FILE_INFO = nil

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
SHAREFLAGS = -G

# default and aliases
# all:	no-clos
all:	partial-clos
compile-CLX-for-CLUE:	compile-partial-clos-CLX
clue:	partial-clos

excldep.so: excldep.c
	-mv excldep.o excldep.ooo
	cc $(CFLAGS) -c $(PICFLAGS) excldep.c
	ld $(SHAREFLAGS) -o excldep.so excldep.o
	rm -f excldep.o
	-mv excldep.ooo excldep.o

socket.so: socket.c
	-mv socket.o socket.ooo
	cc $(CFLAGS) -c $(PICFLAGS) -I/usr/openwin/include socket.c
	ld $(SHAREFLAGS) -o socket.so socket.o
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
	(load \"defsystem\") \
	(si::system-compile-wrapper \
	 (function \
	  (lambda () \
	    (compile-system :clx) \
	    (compile-system :clx-debug))) \
	 :speed $(SPEED) :debug $(DEBUG) :safety $(SAFETY) \
	 :record-source-file-info $(RECORD_SOURCE_FILE_INFO) \
	 :record-xref-info $(RECORD_XREF_INFO) \
	 :load-source-file-info $(LOAD_SOURCE_FILE_INFO) \
	 :load-xref-info $(LOAD_XREF_INFO) \
	 :compile-print nil :compile-verbose nil \
	 :redefinition-warnings t :gcprint nil)" | $(CL) $(CLOPTS) -batch

compile-partial-clos-CLX:	$(C_OBJS)
	$(ECHO) "(pushnew :clx-ansi-common-lisp *features*) \
	(load \"defsystem\") \
	(load \"package\") \
	(setq xlib::*def-clx-class-use-defclass* '(xlib:window xlib:pixmap xlib:drawable)) \
	(si::system-compile-wrapper \
	 (function \
	  (lambda () \
	    (compile-system :clx) \
	    (compile-system :clx-debug))) \
	 :speed $(SPEED) :debug $(DEBUG) :safety $(SAFETY) \
	 :record-source-file-info $(RECORD_SOURCE_FILE_INFO) \
	 :record-xref-info $(RECORD_XREF_INFO) \
	 :load-source-file-info $(LOAD_SOURCE_FILE_INFO) \
	 :load-xref-info $(LOAD_XREF_INFO) \
	 :compile-print nil :compile-verbose nil \
	 :redefinition-warnings t :gcprint nil)" | $(CL) $(CLOPTS) -batch

compile-full-clos-CLX:	$(C_OBJS)
	$(ECHO) "(pushnew :clx-ansi-common-lisp *features*) \
	(load \"defsystem\") \
	(load \"package\") \
	(setq xlib::*def-clx-class-use-defclass* t) \
	(si::system-compile-wrapper \
	 (function \
	  (lambda () \
	    (compile-system :clx) \
	    (compile-system :clx-debug))) \
	 :speed $(SPEED) :debug $(DEBUG) :safety $(SAFETY) \
	 :record-source-file-info $(RECORD_SOURCE_FILE_INFO) \
	 :record-xref-info $(RECORD_XREF_INFO) \
	 :load-source-file-info $(LOAD_SOURCE_FILE_INFO) \
	 :load-xref-info $(LOAD_XREF_INFO) \
	 :compile-print nil :compile-verbose nil \
	 :redefinition-warnings t :gcprint nil)" | $(CL) $(CLOPTS) -batch

cat:
	-cat $(L_NOMACROS_OBJS) > CLX.fasl

load-CLX:
	$(ECHO) "\
	(load-application \
	  (progn (load \"defsystem\") (load-system :clx)) \
	  :global-gc t :devel nil)" \
	'(dumplisp :name "$(CLX)" :checkpoint nil)' \
	"(exit)" | $(CL) $(CLOPTS)

clean_OS:
	rm -f $(C_OBJS) $(C_SOBJS) 

clean:
	$(RM) -f *.fasl debug/*.fasl $(CLX) core $(C_OBJS) $(C_SOBJS) make.out

install_OS:
	mv *.o $(DEST)
	if test -f socket.so; then \
		mv *.so $(DEST); \
	fi

install: install_OS
	mv CLX.fasl $(DEST)/clx.fasl

tags:
	$(TAGS) $(L_SRC) $(C_SRC)
