lib_LTLIBRARIES = libdino.la d_mpi.la d_ieee.la d_ipcerr.la d_socket.la
libdino_la_SOURCES = d_dino.h d_errors.h d_common.h d_built.h d_yacc.h d_runtab.h d_bcio.h\
		     d_types.h d_context.h d_inference.h d_conv.h d_udb.h d_func.h d_eval.h\
		     d_errors.c d_heap.c d_runtab.c\
   		     d_context.c d_inference.c d_bcio.c d_conv.c d_udb.c d_func.c\
		     d_eval.c d_dino.c
nodist_libdino_la_SOURCES = d_yacc.c d_ir.c d_ir.h d_bc.c d_bc.h d_run.c d_run.h d_extern.h
CC = @CC@
libdino_la_CFLAGS = -I$(srcdir)/../ONIGURUMA -I$(srcdir)/../AMMUNITION\
        	    -DDINO_VERSION=$(VERSION) -DCC='"$(CC)"'\
		    -DSTANDARD_DINO_INCLUDE_DIRECTORY='"$(includedir)"'\
		    -DSTANDARD_DINO_EXTERN_LIBS='"$(DINO_EXTERN_LIBS_PATH)"'\
		    -DSTANDARD_DINO_LIB_DIRECTORY='"$(libdir)"'\
		    -DSTANDARD_DINO_LIB_NAME='"$(DINO_LIB_NAME)"'
libdino_la_LDFLAGS = -release $(VERSION) 
bin_PROGRAMS = dino
dino_SOURCES = d_main.c
CLEANFILES = sgramm.c d_yacc.c d_ir.c d_ir.h d_bc.c d_bc.h d_run.c d_run.h d_kw.c d_enc.c d_bcn.c d_bcf.c
dino_DEPENDENCIES = libdino.la
dino_CFLAGS = $(libdino_la_CFLAGS)
dino_LDFLAGS = -lgmp -lm -export-dynamic -static libdino.la ../ONIGURUMA/libonig.la ../AMMUNITION/libcocom.la
d_mpi_la_SOURCES = d_mpi.c
d_mpi_la_CFLAGS = -I. -I$(srcdir) -I$(srcdir)/../AMMUNITION
d_mpi_la_LDFLAGS = -shared -module ../AMMUNITION/libcocom.la
nodist_d_mpi_la_SOURCES = extern.h
d_ieee_la_SOURCES = d_ieee.c
d_ieee_la_CFLAGS = -DIEEE_QUAD -I. -I$(srcdir) -I$(srcdir)/../AMMUNITION
d_ieee_la_LDFLAGS = -shared -module ../AMMUNITION/libcocom.la
nodist_d_ieee_la_SOURCES = extern.h
d_ipcerr_la_SOURCES = d_ipcerr.c
d_ipcerr_la_CFLAGS = -I. -I$(srcdir)
d_ipcerr_la_LDFLAGS = -shared -module
nodist_d_ipcerr_la_SOURCES = extern.h
d_socket_la_SOURCES = d_socket.c
d_socket_la_CFLAGS = -I. -I$(srcdir)
d_socket_la_LDFLAGS = -shared -module
nodist_d_socket_la_SOURCES = extern.h
EXTRA_DIST =  d_yacc.y d_ir.sprut d_bc.sprut d_extern.sprut d_run.sprut\
	      d_kw.shilka d_enc.shilka d_bcn.shilka d_bcf.shilka dino.tst.in compare.tst\
	      README INSTALL d_minimize.d mpi.d ieee.d ipcerr.d socket.d\
	      UnicodeData.txt process_ucodedb.d\
	      dino.sgml.in dino.txt dino.info dino.html dino-1.html dino-2.html\
	      dino-3.html dino-4.html dino-5.html dino-6.html dino-7.html\
	      dino-8.html dino-9.html dino-10.html dino-11.html\
	      dino.pdf dino.dvi dino.ps
dist_man_MANS = dino.1
ACLOCAL_AMFLAGS = -I m4
SPRUT = ../SPRUT/sprut
SHILKA = ../SHILKA/shilka
MSTA = ../MSTA/msta
dinoincludedir = $(includedir)/dino-$(VERSION)
check : test

all-local: d_gen.i doc
dist-local: doc

install-data-local:
	-mkdir -p $(docdir)
	$(INSTALL_DATA) $(srcdir)/dino.txt $(srcdir)/dino*.html\
          $(srcdir)/dino.info* $(srcdir)/dino.pdf\
          $(srcdir)/dino.dvi $(srcdir)/dino.ps\
	  $(docdir)
	-mkdir -p $(dinoincludedir)
	$(INSTALLDATA) d_extern.h $(dinoincludedir)
	$(INSTALLDATA) d_config.h $(dinoincludedir)
	$(INSTALLDATA) $(srcdir)/d_types.h $(dinoincludedir)
	$(INSTALL_DATA) d_gen.i $(dinoincludedir)
	
uninstall-local:
	rm -f $(dinoincludedir)/d_extern.h
	rm -f $(dinoincludedir)/d_config.h
	rm -f $(dinoincludedir)/d_types.h
	rm -f $(dinoincludedir)/d_gen.i
	-rmdir $(dinoincludedir)
	rm -f $(docdir)/dino.txt $(docdir)/dino*.html\
          $(docdir)/dino.info* $(docdir)/dino.pdf\
          $(docdir)/dino.dvi $(docdir)/dino.ps
	-rmdir $(docdir)

# Delete all not needed for distribution.
distclean-local:
	rm -f dino.sgml dino.tmp dino.tex dino.log dino.aux dino.toc quality.tst dino.tst dino.out

# It is too complicated to provide dependencies on file-level.  So we use brute-force approach.
all: d_ir.h d_bc.h d_run.h d_extern.h d_kw.c d_enc.c d_bcn.c d_bcf.c

d_gen.i : dino d_genheader.i $(srcdir)/d_minimize.d
	@if test d_gen.i -ot d_genheader.i;then\
           echo C code Generation Header Minimization.  It can take a while ...;\
           $(EGREP) -v '^#|^$$' d_genheader.i | time ./dino $(srcdir)/d_minimize.d "$(CC)" > d_gen.tmp && mv d_gen.tmp $@;\
        fi

d_genheader.i: $(srcdir)/d_gen.c $(srcdir)/d_common.h $(srcdir)/d_types.h $(srcdir)/d_conv.h\
	       $(srcdir)/d_runtab.h $(srcdir)/d_func.h $(srcdir)/d_eval.h $(srcdir)/d_errors.h\
               $(srcdir)/d_dino.h d_config.h d_run.h d_bc.h\
               $(srcdir)/../AMMUNITION/allocate.h $(srcdir)/../AMMUNITION/vlobject.h \
	       $(srcdir)/../AMMUNITION/position.h $(srcdir)/../AMMUNITION/hashtab.h \
	       $(srcdir)/../AMMUNITION/ticker.h $(srcdir)/../AMMUNITION/bits.h \
	       $(srcdir)/../AMMUNITION/errors.h $(srcdir)/../AMMUNITION/objstack.h
	$(CC) $(libdino_la_CFLAGS) @C_DEBUG_DEFINE@  -E > $@ $(srcdir)/d_gen.c

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

d_kw.c : $(srcdir)/d_kw.shilka
	$(SHILKA) -no-definitions -strip $(srcdir)/d_kw.shilka

d_enc.c : $(srcdir)/d_enc.shilka
	$(SHILKA) -pENC_ -no-definitions -strip $(srcdir)/d_enc.shilka

d_bcn.c : $(srcdir)/d_bcn.shilka
	$(SHILKA) -pNR_ -no-definitions -strip $(srcdir)/d_bcn.shilka

d_bcf.c : $(srcdir)/d_bcf.shilka
	$(SHILKA) -pFR_ -strip $(srcdir)/d_bcf.shilka

d_yacc.c: $(srcdir)/d_yacc.y
	@echo must be - conflicts: 9 shift/reduce.
	$(MSTA) -no-regular-optimization -local-error-recovery $(srcdir)/d_yacc.y

prepare-unicode: $(srcdir)/UnicodeDB.c

$(srcdir)/UnicodeDB.c : $(srcdir)/UnicodeData.txt
	./dino $(srcdir)/process_ucodedb.d $(srcdir)/UnicodeData.txt >UDB.c && mv UDB.c $@

#allocate.h : $(srcdir)/../AMMUNITION/allocate.h
#	   cp $< $@
#vlobject.h: $(srcdir)/../AMMUNITION/vlobject.h
#	    cp $< $@
#position.h: $(srcdir)/../AMMUNITION/position.h
#	    cp $< $@
#hashtab.h: $(srcdir)/../AMMUNITION/hashtab.h
#	   cp $< $@
#spset.h: $(srcdir)/../AMMUNITION/spset.h
#	 cp $< $@
#cspset.h: $(srcdir)/../AMMUNITION/cspset.h
#	  cp $< $@
#objstack.h: $(srcdir)/../AMMUNITION/objstack.h
#	    cp $< $@
#errors.h: $(srcdir)/../AMMUNITION/errors.h
#	  cp $< $@
#commline.h: $(srcdir)/../AMMUNITION/commline.h
#	    cp $< $@
#ticker.h: $(srcdir)/../AMMUNITION/ticker.h
#	  cp $< $@
#bits.h: $(srcdir)/../AMMUNITION/bits.h
#	cp $< $@
#yaep.h: $(srcdir)/../AMMUNITION/yaep.h
#	cp $< $@
#allocate.c : $(srcdir)/../AMMUNITION/allocate.c
#	   cp $< $@
#vlobject.c: $(srcdir)/../AMMUNITION/vlobject.c
#	    cp $< $@
#position.c: $(srcdir)/../AMMUNITION/position.c
#	    cp $< $@
#hashtab.c: $(srcdir)/../AMMUNITION/hashtab.c
#	   cp $< $@
#spset.c: $(srcdir)/../AMMUNITION/spset.c
#	 cp $< $@
#cspset.c: $(srcdir)/../AMMUNITION/cspset.c
#	  cp $< $@
#objstack.c: $(srcdir)/../AMMUNITION/objstack.c
#	    cp $< $@
#errors.c: $(srcdir)/../AMMUNITION/errors.c
#	  cp $< $@
#commline.c: $(srcdir)/../AMMUNITION/commline.c
#	    cp $< $@
#ticker.c: $(srcdir)/../AMMUNITION/ticker.c
#	  cp $< $@
#bits.c: $(srcdir)/../AMMUNITION/bits.c
#	cp $< $@
#yaep.c: $(srcdir)/../AMMUNITION/yaep.c
#	cp $< $@

test : test-dino compare-only-dino

test-dino : dino
	@echo ++++ Test dino interpriter ++++
	$(SHELL) dino.tst

compare-only-dino : dino
	DINO_ONLY=y $(SHELL) $(srcdir)/compare.tst

compare-dino : dino
	@echo ++++ Comparison of different script language implementations ++++
	$(SHELL) $(srcdir)/compare.tst

# Creation of documentation

doc : dino.sgml $(srcdir)/dino.txt $(srcdir)/dino.html\
      $(srcdir)/dino.info $(srcdir)/dino.pdf $(srcdir)/dino.dvi $(srcdir)/dino.ps

dino.sgml : $(srcdir)/dino.sgml.in $(srcdir)/dino.1
	nroff -man $(srcdir)/dino.1 | $(AWK) '{gsub (/.\b/,"");gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");print;}' > dino.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "dino.tmp") > 0)\
                        print line;close("dino.tmp")}\
                 else print;}' $(srcdir)/dino.sgml.in > $@
	rm -f dino.tmp

$(srcdir)/dino.txt: dino.sgml
	linuxdoc -B txt --papersize=letter -f dino.sgml
	mv dino.txt $(srcdir)
	
$(srcdir)/dino.html: dino.sgml
	linuxdoc -B html --papersize=letter dino.sgml
	if test -f dino-1.html;then\
  	  for i in dino-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv dino.html $(srcdir)/dino.html
	
$(srcdir)/dino.info: dino.sgml
	linuxdoc -B info --papersize=letter dino.sgml
	if test -f dino.info-1;then\
	  for i in dino.info-*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv dino.info $(srcdir)/dino.info
	
$(srcdir)/dino.dvi: dino.sgml
	linuxdoc -B latex --papersize=letter dino.sgml
	if test ! -f dino.dvi\
           -o -f dino.tex -a dino.tex -nt dino.dvi; then\
    	        latex dino.tex; latex dino.tex;\
        fi
	mv dino.dvi $(srcdir)/dino.dvi

$(srcdir)/dino.ps: $(srcdir)/dino.dvi
	dvips -o $@ $(srcdir)/dino.dvi

$(srcdir)/dino.pdf: $(srcdir)/dino.ps
	ps2pdf $(srcdir)/dino.ps $@

