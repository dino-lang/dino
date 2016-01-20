lib_LTLIBRARIES = libcocom.la libcocomcpp.la
libcocom_la_SOURCES = allocate.c vlobject.c objstack.c hashtab.c spset.c\
                      cspset.c commline.c position.c errors.c ticker.c\
		      bits.c arithm.c IEEE.c yaep.c
libcocom_la_CFLAGS = @LIB_CFLAGS@ -DIEEE_QUAD
libcocom_la_LDFLAGS = @LIB_LDFLAGS@
libcocomcpp_la_SOURCES = allocatecpp.C vlobjectcpp.C objstackcpp.C hashtabcpp.C spsetcpp.C\
                        cspsetcpp.C commlinecpp.C positioncpp.C errorscpp.C tickercpp.C\
			bitscpp.C arithmcpp.C IEEEcpp.C yaepcpp.C 
libcocomcpp_la_CXXFLAGS = @LIB_CFLAGS@ -DIEEE_QUAD
libcocomcpp_la_LDFLAGS = @LIB_LDFLAGS@
include_HEADERS = allocate.h vlobject.h objstack.h hashtab.h spset.h cspset.h \
                  commline.h position.h errors.h ticker.h bits.h arithm.h IEEE.h yaep.h 
BUILT_SOURCES = sgramm.c
CLEANFILES = sgramm.c
EXTRA_DIST = IEEEtens.c sgramm.y allocate.tst.in vlobject.tst.in objstack.tst.in hashtab.tst.in\
             spset.tst.in cspset.tst.in commline.tst.in position.tst.in errors.tst.in\
	     ticker.tst.in bits.tst.in arithm.tst.in IEEE.tst.in yaep.tst.in\
	     allocate++.tst.in vlobject++.tst.in objstack++.tst.in hashtab++.tst.in\
             spset++.tst.in cspset++.tst.in commline++.tst.in position++.tst.in errors++.tst.in\
	     ticker++.tst.in bits++.tst.in arithm++.tst.in IEEE++.tst.in yaep++.tst.in\
	     compare_parsers.tst.in whole_gcc_test.i ammunition.sgml.in ammunition++.sgml.in\
	     ammunition.txt ammunition.ps ammunition.pdf ammunition.dvi ammunition.info\
	     ammunition.html ammunition-1.html ammunition-2.html ammunition-3.html ammunition-4.html\
	     ammunition-5.html ammunition-6.html ammunition-7.html ammunition-8.html ammunition-9.html\
             ammunition-10.html ammunition-11.html ammunition-12.html\
	     ammunition-13.html ammunition-14.html ammunition-15.html\
	     ammunition++.txt ammunition++.ps ammunition++.pdf ammunition++.dvi ammunition++.info\
	     ammunition++.html ammunition++-1.html ammunition++-2.html ammunition++-3.html ammunition++-4.html\
	     ammunition++-5.html ammunition++-6.html ammunition++-7.html ammunition++-8.html ammunition++-9.html\
             ammunition++-10.html ammunition++-11.html ammunition++-12.html\
	     ammunition++-13.html ammunition++-14.html ammunition++-15.html
ACLOCAL_AMFLAGS = -I m4

check : test

all-local:
dist-local:

install-data-local:
	mkdir -p $(docdir)
	$(INSTALL_DATA) $(srcdir)/ammunition.txt $(srcdir)/ammunition*.html\
          $(srcdir)/ammunition.info* $(srcdir)/ammunition.pdf\
          $(srcdir)/ammunition.dvi $(srcdir)/ammunition.ps\
          $(srcdir)/ammunition++.txt\
          $(srcdir)/ammunition++.info* $(srcdir)/ammunition++.pdf\
          $(srcdir)/ammunition++.dvi $(srcdir)/ammunition++.ps\
	  $(docdir)
	
uninstall-local:
	rm -f $(docdir)/ammunition.txt $(docdir)/ammunition*.html\
          $(docdir)/ammunition.info* $(docdir)/ammunition.pdf\
          $(docdir)/ammunition.dvi $(docdir)/ammunition.ps\
          $(docdir)/ammunition++.txt $(docdir)/ammunition++*.html\
          $(docdir)/ammunition++.info* $(docdir)/ammunition++.pdf\
          $(docdir)/ammunition++.dvi $(docdir)/ammunition++.ps
	-rmdir $(docdir)

# Delete all not needed for distribution.
distclean-local:
	rm -f allocate.tst vlobject.tst objstack.tst hashtab.tst
	rm -f spset.tst cspset.tst
	rm -f commline.tst position.tst errors.tst ticker.tst bits.tst
	rm -f arithm.tst IEEE.tst yaep.tst
	rm -f allocate++.tst vlobject++.tst objstack++.tst hashtab++.tst
	rm -f spset++.tst cspset++.tst
	rm -f commline++.tst position++.tst errors++.tst ticker++.tst
	rm -f bits++.tst arithm++.tst IEEE++.tst yaep++.tst
	rm -f compare_parsers.tst
	rm -f ammunition.sgml ammunition.tex ammunition.log\
              ammunition.aux ammunition.toc ammunition.tmp ammunition.out
	rm -f ammunition++.sgml ammunition++.tex ammunition++.log\
	      ammunition++.aux ammunition++.toc ammunition++.tmp ammunition++.out\
              ammunition.sgml ammunition++.sgml 

sgramm.c: $(srcdir)/sgramm.y
	$(YACC) $(srcdir)/sgramm.y
	mv y.tab.c $@

doc : ammunition.sgml ammunition++.sgml\
      $(srcdir)/ammunition.txt $(srcdir)/ammunition.html\
      $(srcdir)/ammunition.info $(srcdir)/ammunition.pdf\
      $(srcdir)/ammunition.dvi $(srcdir)/ammunition.ps\
      $(srcdir)/ammunition++.txt $(srcdir)/ammunition++.html\
      $(srcdir)/ammunition++.info $(srcdir)/ammunition++.pdf\
      $(srcdir)/ammunition++.dvi $(srcdir)/ammunition++.ps

ammunition.sgml: $(srcdir)/ammunition.sgml.in $(srcdir)/yaep_part.sgml.in
	$(AWK) "{if(\$$0 == \"%%%\")\
                     {while ((getline line < \"$(srcdir)/yaep_part.sgml.in\") > 0)\
                        print line;close(\"$(srcdir)/yaep_part.sgml.in\")}\
                 else print;}" $(srcdir)/ammunition.sgml.in > $@

$(srcdir)/ammunition.txt: ammunition.sgml
	linuxdoc -B txt --papersize=letter -f ammunition.sgml
	mv ammunition.txt $(srcdir)
	
$(srcdir)/ammunition.html: ammunition.sgml
	linuxdoc -B html --papersize=letter ammunition.sgml
	if test -f ammunition-1.html;then\
  	  for i in ammunition-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv ammunition.html $(srcdir)/ammunition.html
	
$(srcdir)/ammunition.info: ammunition.sgml
	linuxdoc -B info --papersize=letter ammunition.sgml
	if test -f ammunition.info;then\
	  for i in ammunition.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	
$(srcdir)/ammunition.dvi: ammunition.sgml
	linuxdoc -B latex --papersize=letter ammunition.sgml
	if test ! -f ammunition.dvi\
           -o -f ammunition.tex -a ammunition.tex -nt ammunition.dvi; then\
    	        latex ammunition.tex; latex ammunition.tex;\
        fi
	mv ammunition.dvi $(srcdir)/ammunition.dvi

$(srcdir)/ammunition.ps: $(srcdir)/ammunition.dvi
	dvips -o $@ $(srcdir)/ammunition.dvi

$(srcdir)/ammunition.pdf: $(srcdir)/ammunition.ps
	ps2pdf $(srcdir)/ammunition.ps $@

ammunition++.sgml: $(srcdir)/ammunition++.sgml.in $(srcdir)/yaep_part++.sgml.in
	$(AWK) "{if(\$$0 == \"%%%\")\
                     {while ((getline line < \"$(srcdir)/yaep_part++.sgml.in\") > 0)\
                        print line;close(\"$(srcdir)/yaep_part++.sgml.in\")}\
                 else print;}" $(srcdir)/ammunition++.sgml.in > $@

$(srcdir)/ammunition++.txt: ammunition++.sgml
	linuxdoc -B txt --papersize=letter -f ammunition++.sgml
	mv ammunition++.txt $(srcdir)

$(srcdir)/ammunition++.html: ammunition++.sgml
	linuxdoc -B html --papersize=letter ammunition++.sgml
	if test -f ammunition++-1.html;then\
  	  for i in ammunition++-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv ammunition++.html $(srcdir)/ammunition++.html
	
$(srcdir)/ammunition++.info: ammunition++.sgml
	linuxdoc -B info --papersize=letter ammunition++.sgml
	if test -f ammunition++.info;then\
	  for i in ammunition++.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	
$(srcdir)/ammunition++.dvi: ammunition++.sgml
	linuxdoc -B latex --papersize=letter ammunition++.sgml
	if test ! -f ammunition++.dvi\
           -o -f ammunition++.tex -a ammunition++.tex -nt ammunition++.dvi; then\
    	        latex ammunition++.tex; latex ammunition++.tex;\
        fi
	mv ammunition++.dvi $(srcdir)/ammunition++.dvi

$(srcdir)/ammunition++.ps: $(srcdir)/ammunition++.dvi
	dvips -o $@ $(srcdir)/ammunition++.dvi

$(srcdir)/ammunition++.pdf: $(srcdir)/ammunition++.ps
	ps2pdf $(srcdir)/ammunition++.ps $@

# Generation of the most precise floating point representation of
# powers of ten (max power (5010) > log10 (2) * (max power of IEEE quad
# number (16384) + 2 * fraction size of IEEE quad numbers (2*113 = 226)
# + minimal bits needed for representation of hundred (7))).  See also
# commentaries for in file `IEEE.c'.
IEEEtens.c :
	if test -f $(srcdir)/IEEEtens.c; then\
	  cp $(srcdir)/IEEEtens.c IEEEtens.c;\
	else\
	  echo \
	  | $(AWK) 'BEGIN {for(i=0;i<=5020;i++)\
                           print "echo \"obase=2;j=1;for(i=0;i<=", i-1,\
                           ";i++)j*=10;j;\"|bc";}'\
	  | $(SHELL)\
	  | $(AWK) '/\\$$/ {s = s substr($$0, 1, length - 1);}\
	            $$0 !~ /\\$$/ {print s $$0; s = "";}'\
	  | $(AWK) '{FractionSize = 15;\
                     exponent = length - 1;\
	             result = "  {"  exponent  ", ";\
		     NLastZeros = NR - 1;\
		     if (length - NLastZeros > FractionSize * 8)\
		       result = result  "1, {";\
		     else\
		       result = result  "0, {";\
	             for (i = 0; i < FractionSize; i++){\
	               if (i != 0) result = result ", ";\
	               result = result "0x";\
		       for (j = 0; j < 2; j++){\
	                 xdigit = substr ($$0, (2 * i + j) * 4 + 1, 4);\
	                 xdigit = xdigit substr ("0000", length (xdigit) + 1);\
	                 if (xdigit == "0000") xdigit = "0";\
		         else if (xdigit == "0001") xdigit = "1";\
		         else if (xdigit == "0010") xdigit = "2";\
		         else if (xdigit == "0011") xdigit = "3";\
		         else if (xdigit == "0100") xdigit = "4";\
		         else if (xdigit == "0101") xdigit = "5";\
		         else if (xdigit == "0110") xdigit = "6";\
		         else if (xdigit == "0111") xdigit = "7";\
	                 else if (xdigit == "1000") xdigit = "8";\
		         else if (xdigit == "1001") xdigit = "9";\
		         else if (xdigit == "1010") xdigit = "a";\
		         else if (xdigit == "1011") xdigit = "b";\
		         else if (xdigit == "1100") xdigit = "c";\
		         else if (xdigit == "1101") xdigit = "d";\
		         else if (xdigit == "1110") xdigit = "e";\
		         else if (xdigit == "1111") xdigit = "f";\
		         else xdigit = "$$";\
	                 result = result xdigit;\
	               }\
	             }\
        	     print result "}},";}'\
	  | $(AWK) '{print $$0;if (NR==370)print "#ifdef IEEE_QUAD"}\
		     END {print "#endif"}' > $@;\
	fi

# Test the packages.
test: test-c test-cpp

test-c: IEEEtens.c allocate-tst vlobject-tst objstack-tst \
        hashtab-tst spset-tst cspset-tst commline-tst position-tst errors-tst \
        ticker-tst bits-tst arithm-tst IEEE-tst yaep-tst
allocate-tst:
	$(SHELL) allocate.tst
vlobject-tst:
	$(SHELL) vlobject.tst
objstack-tst:
	$(SHELL) objstack.tst
hashtab-tst:
	$(SHELL) hashtab.tst
spset-tst:
	$(SHELL) spset.tst
cspset-tst:
	$(SHELL) cspset.tst
commline-tst:
	$(SHELL) commline.tst
position-tst:
	$(SHELL) position.tst
errors-tst:
	$(SHELL) errors.tst
ticker-tst:
	$(SHELL) ticker.tst
bits-tst:
	$(SHELL) bits.tst
arithm-tst:
	$(SHELL) arithm.tst
IEEE-tst:
	$(SHELL) IEEE.tst
yaep-tst:
	$(SHELL) yaep.tst

test-cpp: IEEEtens.c allocatecpp-tst vlobjectcpp-tst objstackcpp-tst \
          hashtabcpp-tst spsetcpp-tst cspsetcpp-tst commlinecpp-tst \
          positioncpp-tst errorscpp-tst tickercpp-tst bitscpp-tst \
          arithmcpp-tst IEEEcpp-tst yaepcpp-tst
allocatecpp-tst:
	$(SHELL) allocate++.tst
vlobjectcpp-tst:
	$(SHELL) vlobject++.tst
objstackcpp-tst:
	$(SHELL) objstack++.tst
hashtabcpp-tst:
	$(SHELL) hashtab++.tst
spsetcpp-tst:
	$(SHELL) spset++.tst
cspsetcpp-tst:
	$(SHELL) cspset++.tst
commlinecpp-tst:
	$(SHELL) commline++.tst
positioncpp-tst:
	$(SHELL) position++.tst
errorscpp-tst:
	$(SHELL) errors++.tst
tickercpp-tst:
	$(SHELL) ticker++.tst
bitscpp-tst:
	$(SHELL) bits++.tst
arithmcpp-tst:
	$(SHELL) arithm++.tst
IEEEcpp-tst:
	$(SHELL) IEEE++.tst
yaepcpp-tst:
	$(SHELL) yaep++.tst
