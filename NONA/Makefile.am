# Cygwin can not find dll without changing PATH
PATH := ${PATH}:../AMMUNITION/.libs

bin_PROGRAMS = nona
nona_SOURCES = common.h common.c tab.h tab.c yacc.h anal.h anal.c gen.h gen.c main.c
nodist_nona_SOURCES = yacc.c ird.c ird.h
CLEANFILES = yacc.c ird.c ird.h
nona_CFLAGS = -I$(srcdir)/../AMMUNITION
nona_LDFLAGS = -static ../AMMUNITION/libcocom.la
dist_man_MANS = nona.1
dist_doc_DATA = nona.txt nona.info nona.html nona-1.html nona-2.html nona-3.html nona-4.html nona-5.html\
   	        nona.pdf nona.dvi nona.ps
EXTRA_DIST =  yacc.y ird.sprut a29k.nona README INSTALL nona.sgml.in
ACLOCAL_AMFLAGS = -I m4
SPRUT = ../SPRUT/sprut

check : test

all-local:
dist-local:

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

doc :
	nroff -man $(srcdir)/nona.1 | $(AWK) '{gsub (/.\b/,"");gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");print;}' > nona.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "nona.tmp") > 0)\
                        print line;close("nona.tmp")}\
                 else print;}' $(srcdir)/nona.sgml.in > nona.sgml
	rm -f nona.tmp
	linuxdoc -B txt --papersize=letter -f nona.sgml
	mv nona.txt $(srcdir)
	linuxdoc -B html --papersize=letter nona.sgml
	if test -f nona-1.html;then\
  	  for i in nona-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv nona.html $(srcdir)/nona.html
	linuxdoc -B info --papersize=letter nona.sgml
	if test -f nona.info;then\
	  for i in nona.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	linuxdoc -B latex --papersize=letter nona.sgml
	if test ! -f nona.dvi\
           -o -f nona.tex -a nona.tex -nt nona.dvi; then\
    	        latex nona.tex; latex nona.tex;\
        fi
	mv nona.dvi $(srcdir)/nona.dvi
	dvips -o $(srcdir)/nona.ps $(srcdir)/nona.dvi
	ps2pdf $(srcdir)/nona.ps $(srcdir)/nona.pdf

