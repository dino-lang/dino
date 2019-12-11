# Cygwin can not find dll without changing PATH
PATH := ${PATH}:../AMMUNITION/.libs

SUBDIRS = . MODULES

lib_LTLIBRARIES = libdino.la
libdino_la_SOURCES = d_dino.h d_errors.h d_common.h d_built.h d_yacc.h d_runtab.h d_bcio.h\
		     d_types.h d_context.h d_inference.h d_conv.h d_udb.h d_func.h d_eval.h\
		     mum.h d_strtab.h d_errors.c d_heap.c d_runtab.c\
   		     d_context.c d_inference.c d_bcio.c d_conv.c d_udb.c d_func.c\
		     d_eval.c d_dino.c
nodist_libdino_la_SOURCES = d_yacc.c d_ir.c d_ir.h d_bc.c d_bc.h d_run.c d_run.h d_extern.h
CC = @CC@
DINO_VERSION=$(VERSION)
DINO_LANG_VERSION=@DINO_LANG_VERSION@
DINO_VERSION_SUFFIX=@DINO_VERSION_SUFFIX@
DINO_MODULE_VERSION_SUFFIX=@DINO_MODULE_VERSION_SUFFIX@
DINO_MODULE_SUFFIX=@DINO_MODULE_SUFFIX@
dino_module_full_suffix=$(DINO_MODULE_VERSION_SUFFIX)$(DINO_MODULE_SUFFIX)
mpi_lib=d_mpi$(dino_module_full_suffix)
ieee_lib=d_ieee$(dino_module_full_suffix)
ipcerr_lib=d_ipcerr$(dino_module_full_suffix)
socket_lib=d_socket$(dino_module_full_suffix)
DINO_EXTERN_LIBS_PATH=\"$(mpi_lib):$(ieee_lib):$(ipcerr_lib):$(socket_lib)\"
libdino_la_CFLAGS = -I$(srcdir)/../ONIGURUMA/src -I$(srcdir)/../ONIGURUMA -I$(srcdir)/../AMMUNITION\
        	    @DINO_LIB_CFLAGS@ -DCC='"$(CC)"'\
		    -DDINO_VERSION=$(DINO_VERSION) -DDINO_LANG_VERSION=$(DINO_LANG_VERSION)\
                    -DSTANDARD_DINO_INCLUDE_DIRECTORY='"$(includedir)"'\
		    -DSTANDARD_DINO_EXTERN_LIBS=$(DINO_EXTERN_LIBS_PATH)\
		    -DSTANDARD_DINO_LIB_DIRECTORY='"$(libdir)"'\
		    -DSTANDARD_DINO_BIN_DIRECTORY='"$(bindir)"'\
		    -DDINO_VERSION_SUFFIX='"$(DINO_VERSION_SUFFIX)"'
libdino_la_LDFLAGS = @DINO_LIB_LDFLAGS@ -release $(DINO_VERSION)
bin_PROGRAMS = dino
dino_SOURCES = d_main.c
CLEANFILES = sgramm.c d_yacc.c d_ir.c d_ir.h d_bc.c d_bc.h d_run.c d_run.h
dino_DEPENDENCIES = libdino.la
dino_CFLAGS = $(libdino_la_CFLAGS)
dino_LDFLAGS = -export-dynamic @DINO_LIB_LDFLAGS@ \
               libdino.la ../ONIGURUMA/src/libonig.la ../AMMUNITION/libcocom.la -lgmp -lm
BUILT_SOURCES = d_ir.h d_bc.h d_run.h
dist_doc_DATA = dino.txt dino.info dino.html dino-1.html dino-2.html\
	        dino-3.html dino-4.html dino-5.html dino-6.html dino-7.html\
	        dino-8.html dino-9.html dino-10.html dino-11.html\
	        dino.pdf dino.dvi dino.ps
dist_man_MANS = dino.1
nodist_include_HEADERS = d_api.h d_gen.i
EXTRA_DIST =  d_gen.c d_yacc.y d_ir.sprut d_bc.sprut d_extern.sprut d_run.sprut\
	      dino.tst.in UnicodeDB.c\
	      compare.tst README INSTALL d_minimize.d d_flatten.d\
	      UnicodeOriginalData.txt process_ucodedb.d\
	      dino.sgml.in
ACLOCAL_AMFLAGS = -I m4
SPRUT = ../SPRUT/sprut
SHILKA = ../SHILKA/shilka
MSTA = ../MSTA/msta
check : test

clean-local:
	rm -f d_gen.tmp d_api.tmp

# Delete all not needed for distribution.
distclean-local: clean-local
	rm -f d_genheader.i d_extern.h d_api.h d_gen.i\
	      dino.sgml dino.tmp dino.tex dino.log dino.aux dino.toc quality.tst dino.tst dino.out

d_api.h: dino$(EXEEXT) d_extern.h d_config.h $(srcdir)/d_types.h $(srcdir)/d_flatten.d
	@if test d_api.h -ot d_extern.h;then\
           echo Flatten d_extern.h;\
           ./dino $(srcdir)/d_flatten.d d_extern.h $(srcdir) > d_api.tmp && mv d_api.tmp $@;\
        fi

d_gen.i : dino$(EXEEXT) d_genheader.i $(srcdir)/d_minimize.d
	@if test d_gen.i -ot d_genheader.i;then\
           echo C code Generation Header Minimization.  It can take a while ...;\
           $(EGREP) -v '^#|^$$' d_genheader.i | ./dino $(srcdir)/d_minimize.d "$(CC)" > d_gen.tmp && mv d_gen.tmp $@;\
        fi

d_genheader.i: $(srcdir)/d_gen.c $(srcdir)/d_common.h $(srcdir)/d_types.h $(srcdir)/d_conv.h\
	       $(srcdir)/d_runtab.h $(srcdir)/d_func.h $(srcdir)/d_eval.h $(srcdir)/d_errors.h\
               $(srcdir)/mum.h $(srcdir)/d_dino.h d_config.h d_run.h d_bc.h\
               $(srcdir)/../AMMUNITION/allocate.h $(srcdir)/../AMMUNITION/vlobject.h \
	       $(srcdir)/../AMMUNITION/position.h $(srcdir)/../AMMUNITION/hashtab.h \
	       $(srcdir)/../AMMUNITION/ticker.h $(srcdir)/../AMMUNITION/bits.h \
	       $(srcdir)/../AMMUNITION/errors.h $(srcdir)/../AMMUNITION/objstack.h
	$(CC) @DEFS@ $(CPPFLAGS) -I$(srcdir) -I. $(libdino_la_CFLAGS) $(CFLAGS) -E > d_gen.tmp $(srcdir)/d_gen.c && mv d_gen.tmp $@

# CYGWIN libtools sometimes wrongly install wrapper program -- correct it
install-exec-hook:
	if uname | fgrep CYGWIN > /dev/null && fgrep '%%%MAGIC variable%%%' $(bindir)/dino$(EXEEXT) >/dev/null; then\
	    $(INSTALL_PROGRAM) .libs/dino$(EXEEXT) $(bindir)/dino$(EXEEXT);\
	fi

sgramm.c: $(srcdir)/../AMMUNITION/sgramm.y
	$(YACC) $(srcdir)/../AMMUNITION/sgramm.y
	mv y.tab.c $@

d_ir.c d_ir.h : $(srcdir)/d_ir.sprut
# Use -debug instead of -only-macro for debugging
	../SPRUT/sprut @START_SPRUT_OPTIONS@ -access -set -copy $(srcdir)/d_ir.sprut

d_bc.c d_bc.h : $(srcdir)/d_bc.sprut
# Use -debug instead of -only-macro for debugging
	../SPRUT/sprut @START_SPRUT_OPTIONS@ -pBC_ -access -set -copy $(srcdir)/d_bc.sprut

d_run.c d_run.h : $(srcdir)/d_extern.sprut $(srcdir)/d_run.sprut
# Use -debug instead of -only-macro for debugging
	../SPRUT/sprut @START_SPRUT_OPTIONS@ -pER_ -access -set -flat-structure $(srcdir)/d_run.sprut

d_extern.c d_extern.h : $(srcdir)/d_extern.sprut
# Use -debug instead of -only-macro for debugging
	../SPRUT/sprut @START_SPRUT_OPTIONS@ -pER_ -access -set -flat-structure $(srcdir)/d_extern.sprut
	rm -f d_extern.c

d_yacc.c: $(srcdir)/d_yacc.y
	@echo must be - conflicts: 9 shift/reduce.
	$(MSTA) -no-regular-optimization -local-error-recovery $(srcdir)/d_yacc.y

prepare-unicode: $(srcdir)/UnicodeDB.c

$(srcdir)/UnicodeDB.c : $(srcdir)/UnicodeOriginalData.txt
	./dino $(srcdir)/process_ucodedb.d $(srcdir)/UnicodeOriginalData.txt >UDB.c && mv UDB.c $@

test : test-dino compare-only-dino

test-dino : dino$(EXEEXT)
	@echo ++++ Test dino interpriter ++++
	$(SHELL) dino.tst

compare-only-dino : dino$(EXEEXT)
	DINO_ONLY=y $(srcdir)/compare.tst

compare-dino : dino$(EXEEXT)
	@echo ++++ Comparison of different script language implementations ++++
	$(srcdir)/compare.tst

# Creation of documentation
doc : 
	nroff -man $(srcdir)/dino.1 | $(AWK) '{gsub (/.\b/,"");gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");print;}' > dino.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "dino.tmp") > 0)\
                        print line;close("dino.tmp")}\
                 else print;}' $(srcdir)/dino.sgml.in > dino.sgml
	rm -f dino.tmp
	linuxdoc -B txt --papersize=letter -f dino.sgml
	mv dino.txt $(srcdir)
	linuxdoc -B html --papersize=letter dino.sgml
	if test -f dino-1.html;then\
  	  for i in dino-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv dino.html $(srcdir)/dino.html
	linuxdoc -B info --papersize=letter dino.sgml
	if test -f dino.info-1;then\
	  for i in dino.info-*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv dino.info $(srcdir)/dino.info
	linuxdoc -B latex --papersize=letter dino.sgml
	if test ! -f dino.dvi\
           -o -f dino.tex -a dino.tex -nt dino.dvi; then\
    	        latex dino.tex; latex dino.tex;\
        fi
	mv dino.dvi $(srcdir)/dino.dvi
	dvips -o $(srcdir)/dino.ps $(srcdir)/dino.dvi
	ps2pdf $(srcdir)/dino.ps $(srcdir)/dino.pdf

# Test installed DINO
test-installed:
	(mkdir __t && cd __t \
	 && $(bindir)/dino$(EXEEXT) -c 'include "mpi"; var m = mpis.from_string(1, "10");' \
	 && cd .. && rm -rf __t)
	(mkdir __t && cd __t \
	 && $(bindir)/dino$(EXEEXT) -s -c 'fun f !jit {} f();' 2>&1 | fgrep 'their calls - 1' \
	 && cd .. && rm -rf __t)