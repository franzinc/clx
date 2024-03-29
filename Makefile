#  Makefile for CLX

SHELL = sh

FICODESIGN = ../src/scm-bin/ficodesign

makefile_top = $(shell if test -f ../makefile.top; then echo exists; fi)

ifeq ($(makefile_top),exists)
include ../makefile.top
include ../makefile.defs
endif

iacl = yes

ifdef iacl
lispexe = lispi$(exe)
lispdxl = dcli.dxl
else
lispexe = lisp$(exe)
lispdxl = dcl.dxl
endif

# For versions prior to ACL 5.0 (and 4.3.2 on Windows), comment out the
# following line:
SAVEIMG = yes

# *************************************************************************
# * Change the next line to point to where you have Common Lisp installed *
# *        (make sure the Lisp doesn't already have CLX loaded in)        *
# *************************************************************************
ifdef SAVEIMG
ifeq ($(OS_NAME),windows)
CL		= bash ../src/runlisp.sh -f clx.tmp ../src/$(lispexe) -I ../src/$(lispdxl)
else
CL		= cat clx.tmp | ../src/$(lispexe) -I ../src/$(lispdxl)
endif
# Name of dumped lisp
CLX		= clx.dxl
DUMPLISP_ARGS	=
else
CL		= cat clx.tmp | ../src/dcl
# Name of dumped lisp
CLX		= clx
DUMPLISP_ARGS	= :checkpoint nil
endif

ifeq ($(OS_NAME),windows)
ECHO	= echo
else
ECHO	= echo
endif
MV	= mv
TAGS	= /usr/local/lib/emacs/etc/etags

CLOPTS	= -qq

SO = so

ifeq ($(OS_NAME),hp-ux)
CC = /usr/bin/cc
else
CC = cc
endif

ifeq ($(OS_NAME),aix)
ifeq ($(SIXTYFOURBIT),yes)
XCFLAGS = -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void -q64 -DAcl64Bit
MAKE_SHARED = sh ../src/bin/make_shared.ibm64 -make_exp ../src/bin/make_exp
else
XCFLAGS = -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void
MAKE_SHARED = sh ../src/bin/make_shared.ibm -make_exp ../src/bin/make_exp
endif
endif

ifeq ($(OS_NAME),hp-ux)
ifeq ($(SIXTYFOURBIT),yes)
XCFLAGS = -O -Ae +DA2.0W -DAcl64Bit
MAKE_SHARED = sh ../src/bin/make_shared.hp64
PICFLAGS = +Z
#CC = aCC
else
XCFLAGS = -O -Ae +DA1.1
SO = sl
MAKE_SHARED = sh ../src/bin/make_shared.hp
PICFLAGS = +z
endif
endif

ifeq ($(OS_NAME),darwin)
PICFLAGS = 
SHAREFLAGS = 
SO = dylib
ifeq ($(SIXTYFOURBIT),yes)
MAKE_SHARED = sh ../src/bin/make_shared.mac64
ifeq ($(MACHINE),x86_64)
XCFLAGS = -I/usr/X11/include -m64 -arch x86_64
else
XCFLAGS = -I/usr/X11/include
endif
else
XCFLAGS = -I/usr/X11/include -arch i386
ifeq ($(MACHINE),x86)
MAKE_SHARED = sh ../src/bin/make_shared.mac86
else
MAKE_SHARED = sh ../src/bin/make_shared.mac
endif

endif
endif

ifeq ($(OS_NAME),sunos)
ifeq ($(SIXTYFOURBIT),yes)
ifeq ($(MACHINE),x86_64)
XCFLAGS = -xarch=amd64 -DAcl64Bit
PICFLAGS = -KPIC
MAKE_SHARED = ld -G
else
XCFLAGS = -xarch=v9 -DAcl64Bit
PICFLAGS = -K pic
MAKE_SHARED = ld -G
endif
else
XCFLAGS = -I/usr/openwin/include
PICFLAGS = -K pic
MAKE_SHARED = ld -G
endif
endif

ifeq ($(OS_NAME),linux)
X11R6exists = $(shell if test -d /usr/X11R6/include; then echo yes; fi)
ifeq ($(X11R6exists),yes)
XCFLAGS = -I/usr/X11R6/include
endif
PICFLAGS = -fPIC
MAKE_SHARED = ld -shared
THREADLIB = -lpthread
ifneq ($(SIXTYFOURBIT),yes)
XCFLAGS += -m32
MAKE_SHARED = ../src/bin/make_shared.lx32
endif
endif

ifeq ($(OS_NAME),freebsd)
x11_include_location := $(shell if test -d /usr/local/include/X11; then echo /usr/local/include; else echo /usr/X11R6/include; fi)
XCFLAGS = -I$(x11_include_location)
PICFLAGS = -fPIC -DPIC
MAKE_SHARED = ld -Bshareable -Bdynamic
endif

ifeq ($(OS_NAME),osf1)
ifeq ($(SIXTYFOURBIT),yes)
XCFLAGS = -G 0 -DAcl64Bit -resumption_safe
MAKE_SHARED = sh ../src/bin/make_shared.dec64
else
XCFLAGS = -G 0 -taso -xtaso -xtaso_short -resumption_safe
MAKE_SHARED = sh ../src/bin/make_shared.dec
endif
endif

ifeq ($(OS_NAME),irix)
XCFLAGS = -G 0 -ansi -n32
PICFLAGS = -KPIC
MAKE_SHARED = ld -n32 -shared -all
endif

CFLAGS	= -O -DUNIXCONN $(XCFLAGS)

# compile options:
SPEED			= 3
SAFETY			= 0
DEBUG			= 1
RECORD_XREF_INFO	= nil
RECORD_SOURCE_FILE_INFO	= nil
SAVE_LOCAL_NAMES	= nil
SAVE_SOURCE_DEBUG	= nil
LOAD_XREF_INFO		= nil
LOAD_SOURCE_FILE_INFO	= nil
LOAD_LOCAL_NAMES_INFO	= nil
gc_print		= nil
compile_verbose		= t
compile_print		= nil
redef_warning		= t

C_SRC	= excldep.c socket.c
ifneq ($(OS_NAME),windows)
C_OBJS	= excldep.o socket.o
C_SOBJS	= excldep.$(SO) socket.$(SO)
endif

L_OBJS	= defsystem.fasl package.fasl excldep.fasl depdefs.fasl clx0.fasl \
	dependent.fasl exclcmac.fasl macros.fasl bufmac.fasl buffer.fasl \
	display.fasl gcontext.fasl requests.fasl input.fasl fonts.fasl \
	graphics.fasl text.fasl attributes.fasl translate.fasl keysyms.fasl \
	manager.fasl image.fasl resource.fasl

L_NOMACROS_OBJS	= package.fasl excldep.fasl depdefs.fasl clx0.fasl \
	dependent.fasl exclcmac.fasl buffer.fasl display.fasl gcontext.fasl \
	requests.fasl input.fasl fonts.fasl graphics.fasl text.fasl \
	attributes.fasl translate.fasl keysyms.fasl manager.fasl image.fasl \
	resource.fasl

L_SRC	= defsystem.cl package.cl excldep.cl depdefs.cl clx0.cl \
	dependent.cl exclcmac.cl macros.cl bufmac.cl buffer.cl \
	display.cl gcontext.cl requests.cl input.cl fonts.cl \
	graphics.cl text.cl attributes.cl translate.cl keysyms.cl \
	manager.cl image.cl resource.cl

all:	$(C_SOBJS) partial-clos

compile-CLX-for-CLUE:	compile-partial-clos-CLX
clue:	partial-clos

excldep.so: excldep.c
	$(CC) $(CFLAGS) -c $(PICFLAGS) excldep.c
	$(MAKE_SHARED) $(THREADLIB) -o excldep.so excldep.o

excldep.sl: excldep.c
	$(CC) $(CFLAGS) -c $(PICFLAGS) excldep.c
	$(MAKE_SHARED) -o excldep.sl excldep.o

excldep.dylib: excldep.c
	$(CC) $(CFLAGS) -c $(PICFLAGS) excldep.c
	$(MAKE_SHARED) -o excldep.dylib excldep.o
	$(FICODESIGN) excldep.dylib 

socket.so: socket.c
	$(CC) $(CFLAGS) -c $(PICFLAGS) socket.c
	$(MAKE_SHARED) $(THREADLIB) -o socket.so socket.o

socket.sl: socket.c
	$(CC) $(CFLAGS) -c $(PICFLAGS) socket.c
	$(MAKE_SHARED) -o socket.sl socket.o

socket.dylib: socket.c
	$(CC) $(CFLAGS) -c $(PICFLAGS) socket.c
	$(MAKE_SHARED) -o socket.dylib socket.o
	$(FICODESIGN) socket.dylib 

#
# Three build rules are provided: no-clos, partial-clos, and full-clos.
# The first is no-clos, which results in a CLX whose datastructures are
# all defstructs.  partial-clos results in xlib:window, xlib:pixmap, and
# xlib:drawable being CLOS instances, all others defstructs.  full-clos
# makes all CLX complex datatypes into CLOS instances.
#
# (note that the :clos feature implies native CLOS *not* PCL).
#

no-clos:	$(C_OBJS) compile-no-clos-CLX clx.fasl

#
# This rule is used to compile CLX to be used with XCW version 2, or CLUE.
#
partial-clos:	$(C_OBJS) compile-partial-clos-CLX clx.fasl

full-clos:	$(C_OBJS) compile-full-clos-CLX clx.fasl


c:	$(C_OBJS)


compile-no-clos-CLX:	$(C_OBJS)
	$(ECHO) "(pushnew :clx-ansi-common-lisp *features*) \
        (push :clx-use-allegro-streams *features*) \
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
	      (comp:save-source-level-debug-info-switch $(SAVE_SOURCE_DEBUG)) \
	      (*compile-print* $(compile_print)) \
	      (*compile-verbose* $(compile_verbose))) \
	    (setf (sys:gsgc-switch :print) $(gc_print)) \
	    (setq excl::*warn-smp-usage* nil) \
	    (compile-system :clx) \
	    (compile-system :clx-debug) \
	    (exit 0))" > clx.tmp
	$(CL) $(CLOPTS) -batch

compile-partial-clos-CLX:	$(C_OBJS)
	$(ECHO) "#-mswindows (ff:get-entry-point (ff:convert-foreign-name \"fd_wait_for_input\")) \
	(pushnew :clx-ansi-common-lisp *features*) \
	(push :clx-use-allegro-streams *features*) \
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
	      (comp:save-source-level-debug-info-switch $(SAVE_SOURCE_DEBUG)) \
	      (*compile-verbose* $(compile_verbose)) \
	      (*compile-print* $(compile_print))) \
	    (setf (sys:gsgc-switch :print) $(gc_print)) \
	    (setq excl::*warn-smp-usage* nil) \
	    (compile-system :clx) \
	    (compile-system :clx-debug) \
	    (exit 0))" > clx.tmp
	$(CL) $(CLOPTS) -batch -backtrace-on-error

compile-full-clos-CLX:	$(C_OBJS)
	$(ECHO) "(pushnew :clx-ansi-common-lisp *features*) \
	(push :clx-use-allegro-streams *features*) \
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
	      (comp:save-source-level-debug-info-switch $(SAVE_SOURCE_DEBUG)) \
	      (*compile-print* $(compile_print)) \
	      (*compile-verbose* $(compile_verbose))) \
	    (setf (sys:gsgc-switch :print) $(gc_print)) \
	    (setq excl::*warn-smp-usage* nil) \
	    (compile-system :clx) \
	    (compile-system :clx-debug) \
	    (exit 0))" > clx.tmp
	$(CL) $(CLOPTS) -batch

clx.fasl:
#The following doesn't work on Windows, so use concatenate-system instead.
#	-cat $(L_NOMACROS_OBJS) > clx.fasl
#The following contains a little more than the above does (same as L_OBJS).
	$(ECHO) "(load-logical-pathname-translations \"clx\") \
	(load \"defsystem\") \
	(concatenate-system :clx \"clx.fasl\") \
	(exit 0)" > clx.tmp
	$(CL) $(CLOPTS) -batch

load-CLX:
	$(ECHO) "\
	(load-logical-pathname-translations \"clx\") \
	(load-application \
	  (progn (load \"defsystem\") (load-system :clx)) \
	  :global-gc t :devel nil)" \
	'(dumplisp :name "$(CLX)" $(DUMPLISP_ARGS))' \
	"(exit 0)" > clx.tmp
	$(CL) $(CLOPTS)

clean_OS:
	rm -f *.o *.so *.sl

clean:
	rm -f *.fasl *.o *.so *.sl *.dylib debug/*.fasl $(CLX) core make.out clx.tmp
	rm -f so_locations

install_OS:
	$(MV) *.o $(DEST)
	if test -f socket.so; then \
		$(MV) *.so $(DEST); \
	fi
	if test -f socket.sl; then \
		$(MV) *.sl $(DEST); \
	fi

install: install_OS
	$(MV) clx.fasl $(DEST)/clx.fasl

tags:
	$(TAGS) $(L_SRC) $(C_SRC)

DISTFILES = CHANGES ChangeLog Makefile */Makefile NEWCHANGES README */README \
	*.c *.cl */*.cl doc/*.Z

echo-distribution-source-files:
	@echo $(DISTFILES)

