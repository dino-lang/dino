# Cygwin can not find dll without changing PATH
PATH := ${PATH}:../AMMUNITION/.libs

bin_PROGRAMS = nona
nona_SOURCES = common.h common.c tab.h tab.c yacc.h anal.h anal.c gen.h gen.c main.c
nodist_nona_SOURCES = yacc.c ird.c ird.h
CLEANFILES = yacc.c ird.c ird.h
nona_CFLAGS = -I$(srcdir)/../AMMUNITION
nona_LDFLAGS = -static ../AMMUNITION/libcocom.la
EXTRA_DIST =  yacc.y ird.sprut a29k.nona README INSTALL nona.sgml.in nona.txt nona.info\
	      nona.html nona-1.html nona-2.html nona-3.html nona-4.html nona-5.html\
	      nona.pdf nona.dvi nona.ps
dist_man_MANS = nona.1
ACLOCAL_AMFLAGS = -I m4
SPRUT = ../SPRUT/sprut

check : test

all-local:
dist-local:

install-data-local:
	-mkdir -p $(docdir)
	$(INSTALL_DATA) $(srcdir)/nona.txt $(srcdir)/nona*.html\
          $(srcdir)/nona.info* $(srcdir)/nona.pdf\
          $(srcdir)/nona.dvi $(srcdir)/nona.ps\
	  $(docdir)
	
uninstall-local:
	rm -f $(docdir)/nona.txt $(docdir)/nona*.html\
          $(docdir)/nona.info* $(docdir)/nona.pdf\
          $(docdir)/nona.dvi $(docdir)/nona.ps
	-rmdir $(docdir)

# Delete all not needed for distribution.
distclean-local:
	rm -f nona.sgml nona.tex nona.log nona.aux nona.toc nona.tmp nona.out

ird.c ird.h : $(srcdir)/ird.sprut
	$(SPRUT) @START_SPRUT_OPTIONS@ -access -set -new -copy $(srcdir)/ird.sprut

yacc.c: $(srcdir)/yacc.y
	$(YACC) $(srcdir)/yacc.y
# This is necessary for MS-DOS YACC (BISON) and VMS POSIX YACC
	if test ! -f y.tab.c && test -f y_tab.c;then mv y_tab.c $@;\
	elif test ! -f y.tab.c && test -f ytab.c;then mv ytab.c $@;\
	else mv y.tab.c $@;fi

nona-common.$(OBJEXT) nona-anal.$(OBJEXT) nona-gen.$(OBJEXT) nona-tab.$(OBJEXT) nona-main.$(OBJEXT) nona-yacc.$(OBJEXT): ird.h

test : nona
	if ./nona -export $(srcdir)/a29k.nona && $(CC) -c a29k.c\
           && ./nona -c++ -export $(srcdir)/a29k.nona && $(CC) -c a29k.cpp;\
        then echo "Test OK"; else echo "Test FAIL!";fi
	rm -f a29k.h a29k.c a29k.cpp

# Creation of documentation

doc : nona.sgml $(srcdir)/nona.txt $(srcdir)/nona.html $(srcdir)/nona.info $(srcdir)/nona.pdf $(srcdir)/nona.dvi $(srcdir)/nona.ps

nona.sgml : $(srcdir)/nona.sgml.in $(srcdir)/nona.1
	nroff -man $(srcdir)/nona.1 | $(AWK) '{gsub (/.\b/,"");gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");print;}' > nona.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "nona.tmp") > 0)\
                        print line;close("nona.tmp")}\
                 else print;}' $(srcdir)/nona.sgml.in > $@
	rm -f nona.tmp

$(srcdir)/nona.txt: nona.sgml
	linuxdoc -B txt --papersize=letter -f nona.sgml
	mv nona.txt $(srcdir)
	
$(srcdir)/nona.html: nona.sgml
	linuxdoc -B html --papersize=letter nona.sgml
	if test -f nona-1.html;then\
  	  for i in nona-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv nona.html $(srcdir)/nona.html
	
$(srcdir)/nona.info: nona.sgml
	linuxdoc -B info --papersize=letter nona.sgml
	if test -f nona.info;then\
	  for i in nona.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi

$(srcdir)/nona.dvi: nona.sgml
	linuxdoc -B latex --papersize=letter nona.sgml
	if test ! -f nona.dvi\
           -o -f nona.tex -a nona.tex -nt nona.dvi; then\
    	        latex nona.tex; latex nona.tex;\
        fi
	mv nona.dvi $(srcdir)/nona.dvi

$(srcdir)/nona.ps: $(srcdir)/nona.dvi
	dvips -o $@ $(srcdir)/nona.dvi

$(srcdir)/nona.pdf: $(srcdir)/nona.ps
	ps2pdf $(srcdir)/nona.ps $@

