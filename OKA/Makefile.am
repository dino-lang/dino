# Cygwin can not find dll without changing PATH
PATH := ${PATH}:../AMMUNITION/.libs:../../AMMUNITION/.libs

bin_PROGRAMS = oka
oka_SOURCES = common.h common.c tab.h tab.c yacc.h anal.h anal.c gen.h gen.c main.c
nodist_oka_SOURCES = yacc.c ird.c ird.h
CLEANFILES = yacc.c ird.c ird.h
oka_CFLAGS = -I$(srcdir)/../AMMUNITION
oka_LDFLAGS = -static ../AMMUNITION/libcocom.la -lm
EXTRA_DIST =  yacc.y ird.sprut alpha-ev4.oka alpha-ev5.oka README INSTALL oka.tst oka.sgml.in\
	      SCHEDULE/schedule.h SCHEDULE/schedtst.c SCHEDULE/schedtst.cpp\
              SCHEDULE/schedule.tst.in SCHEDULE/schedule++.tst.in SCHEDULE/schedule.c\
	      SCHEDULE/schedule.cpp SCHEDULE/test.oka oka.txt oka.info\
	      oka.html oka-1.html oka-2.html oka-3.html oka-4.html oka-5.html\
	      oka-6.html oka-7.html oka-8.html oka-9.html oka.pdf oka.dvi oka.ps
dist_man_MANS = oka.1
ACLOCAL_AMFLAGS = -I m4
SPRUT = ../SPRUT/sprut

check : test

all-local:
dist-local:

install-data-local:
	-mkdir -p $(docdir)
	$(INSTALL_DATA) $(srcdir)/oka.txt $(srcdir)/oka*.html\
          $(srcdir)/oka.info* $(srcdir)/oka.pdf\
          $(srcdir)/oka.dvi $(srcdir)/oka.ps\
	  $(docdir)
	
uninstall-local:
	rm -f $(docdir)/oka.txt $(docdir)/oka*.html\
          $(docdir)/oka.info* $(docdir)/oka.pdf\
          $(docdir)/oka.dvi $(docdir)/oka.ps
	-rmdir $(docdir)

# Delete all not needed for distribution.
distclean-local:
	rm -f SCHEDULE/schedule.tst SCHEDULE/schedule++.tst
	-rmdir SCHEDULE
	rm -f oka.sgml oka.tex oka.log oka.aux oka.toc oka.tmp oka.out

ird.c ird.h : $(srcdir)/ird.sprut
	$(SPRUT) @START_SPRUT_OPTIONS@ -access -set -new -copy -transform $(srcdir)/ird.sprut

yacc.c: $(srcdir)/yacc.y
	$(YACC) $(srcdir)/yacc.y
# This is necessary for MS-DOS YACC (BISON) and VMS POSIX YACC
	if test ! -f y.tab.c && test -f y_tab.c;then mv y_tab.c $@;\
	elif test ! -f y.tab.c && test -f ytab.c;then mv ytab.c $@;\
	else mv y.tab.c $@;fi

oka-common.$(OBJEXT) oka-anal.$(OBJEXT) oka-gen.$(OBJEXT) oka-tab.$(OBJEXT) oka-main.$(OBJEXT) oka-yacc.$(OBJEXT): ird.h

test : test-oka test-sched

test-oka: oka
	$(SHELL) $(srcdir)/oka.tst

test-sched: oka
	(cd SCHEDULE; $(SHELL) schedule.tst; $(SHELL) schedule++.tst)

# Creation of documentation

doc : oka.sgml $(srcdir)/oka.txt $(srcdir)/oka.html\
      $(srcdir)/oka.info $(srcdir)/oka.pdf $(srcdir)/oka.dvi $(srcdir)/oka.ps

oka.sgml : $(srcdir)/oka.sgml.in $(srcdir)/oka.1
	nroff -man $(srcdir)/oka.1 | $(AWK) '{gsub (/.\b/,"");gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");print;}' > oka.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "oka.tmp") > 0)\
                        print line;close("oka.tmp")}\
                 else print;}' $(srcdir)/oka.sgml.in > $@
	rm -f oka.tmp

$(srcdir)/oka.txt: oka.sgml
	linuxdoc -B txt --papersize=letter -f oka.sgml
	mv oka.txt $(srcdir)
	
$(srcdir)/oka.html: oka.sgml
	linuxdoc -B html --papersize=letter oka.sgml
	if test -f oka-1.html;then\
  	  for i in oka-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv oka.html $(srcdir)/oka.html
	
$(srcdir)/oka.info: oka.sgml
	linuxdoc -B info --papersize=letter oka.sgml
	if test -f oka.info;then\
	  for i in oka.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	
$(srcdir)/oka.dvi: oka.sgml
	linuxdoc -B latex --papersize=letter oka.sgml
	if test ! -f oka.dvi\
           -o -f oka.tex -a oka.tex -nt oka.dvi; then\
    	        latex oka.tex; latex oka.tex;\
        fi
	mv oka.dvi $(srcdir)/oka.dvi

$(srcdir)/oka.ps: $(srcdir)/oka.dvi
	dvips -o $@ $(srcdir)/oka.dvi

$(srcdir)/oka.pdf: $(srcdir)/oka.ps
	ps2pdf $(srcdir)/oka.ps $@
