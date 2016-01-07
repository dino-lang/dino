bin_PROGRAMS = shilka
shilka_SOURCES = common.h common.c tab.h tab.c yacc.h anal.h anal.c gen.h gen.c main.c
nodist_shilka_SOURCES = yacc.c ird.c ird.h
CLEANFILES = yacc.c ird.c ird.h
shilka_CFLAGS = -I$(srcdir)/../AMMUNITION
shilka_LDFLAGS = -static ../AMMUNITION/libcocom.la
EXTRA_DIST =  yacc.y ird.sprut shilka.tst.in compare.tst.in\
	      java.shilka ada-standards.shilka ada.shilka b.shilka c.shilka cpp.shilka d.shilka gpc.shilka\
              modula2.shilka modula3.shilka pascal.shilka gcc.shilka gcc.gperf c.gperf\
	      README INSTALL shilka.sgml.in shilka.txt shilka.info shilka.html shilka-1.html shilka-2.html\
	      shilka-3.html shilka-4.html shilka-5.html shilka-6.html shilka-7.html shilka-8.html\
	      shilka.pdf shilka.dvi shilka.ps
dist_man_MANS = shilka.1
ACLOCAL_AMFLAGS = -I m4
SPRUT = ../SPRUT/sprut

check : test

all-local: doc
dist-local: doc

install-data-local:
	-mkdir -p $(docdir)
	$(INSTALL_DATA) $(srcdir)/shilka.txt $(srcdir)/shilka*.html\
          $(srcdir)/shilka.info* $(srcdir)/shilka.pdf\
          $(srcdir)/shilka.dvi $(srcdir)/shilka.ps\
	  $(docdir)
	
uninstall-local:
	rm -f $(docdir)/shilka.txt $(docdir)/shilka*.html\
          $(docdir)/shilka.info* $(docdir)/shilka.pdf\
          $(docdir)/shilka.dvi $(docdir)/shilka.ps
	-rmdir $(docdir)

# Delete all not needed for distribution.
distclean-local:
	rm -f shilka.sgml shilka.tex shilka.log shilka.aux shilka.toc compare.tst shilka.tst shilka.tmp shilka.out

ird.c ird.h : $(srcdir)/ird.sprut
	$(SPRUT) @START_SPRUT_OPTIONS@ -access -set -new\
                       $(srcdir)/ird.sprut

shilka-common.$(OBJEXT) shilka-anal.$(OBJEXT) shilka-gen.$(OBJEXT) shilka-tab.$(OBJEXT) shilka-main.$(OBJEXT) shilka-yacc.$(OBJEXT): ird.h

yacc.c: $(srcdir)/yacc.y
	$(YACC) $(YFLAGS) $(srcdir)/yacc.y
# This is necessary for MS-DOS YACC (BISON) and VMS POSIX YACC
	if test ! -f y.tab.c && test -f y_tab.c;then mv y_tab.c $@;\
	elif test ! -f y.tab.c && test -f ytab.c;then mv ytab.c $@;\
	else mv y.tab.c $@;fi

doc : shilka.sgml $(srcdir)/shilka.txt $(srcdir)/shilka.html $(srcdir)/shilka.info $(srcdir)/shilka.pdf $(srcdir)/shilka.dvi $(srcdir)/shilka.ps

shilka.sgml : $(srcdir)/shilka.sgml.in $(srcdir)/shilka.1
	nroff -man $(srcdir)/shilka.1 | $(AWK) '{gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");gsub (/.\b/,"");print;}' > shilka.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "shilka.tmp") > 0)\
                        print line;close("shilka.tmp")}\
                 else print;}' $(srcdir)/shilka.sgml.in > $@
	rm -f shilka.tmp

$(srcdir)/shilka.txt: shilka.sgml
	linuxdoc -B txt --papersize=letter -f shilka.sgml
	mv shilka.txt $(srcdir)
	
$(srcdir)/shilka.html: shilka.sgml
	linuxdoc -B html shilka.sgml
	if test -f shilka-1.html;then\
  	  for i in shilka-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv shilka.html $(srcdir)/shilka.html
	
$(srcdir)/shilka.info: shilka.sgml
	linuxdoc -B info shilka.sgml
	if test -f shilka.info;then\
	  for i in shilka.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	
$(srcdir)/shilka.dvi: shilka.sgml
	linuxdoc -B latex shilka.sgml
	if test ! -f shilka.dvi\
           -o -f shilka.tex -a shilka.tex -nt shilka.dvi; then\
    	        latex shilka.tex; latex shilka.tex;\
        fi
	mv shilka.dvi $(srcdir)/shilka.dvi

$(srcdir)/shilka.ps: $(srcdir)/shilka.dvi
	dvips -o $@ $(srcdir)/shilka.dvi

$(srcdir)/shilka.pdf: $(srcdir)/shilka.ps
	ps2pdf $(srcdir)/shilka.ps $@

test : shilka
	$(SHELL) ./shilka.tst
	@if type gperf 2>&1|fgrep 'ot found';then\
	  echo +++ Can not compare with gperf because it is not found +++;\
	else\
	  $(SHELL) ./compare.tst;\
	fi
