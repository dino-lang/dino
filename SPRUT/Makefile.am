# Cygwin can not find dll without changing PATH
PATH := ${PATH}:../AMMUNITION/.libs

bin_PROGRAMS = sprut
sprut_SOURCES = anal.c gen.c tab.c main.c common.c anal.h gen.h tab.h common.h yacc.h
nodist_sprut_SOURCES = yacc.c ird.c ird.h A1_base.sprut A1_syntax.sprut A1_value.sprut A1_test.sprut
CLEANFILES = yacc.c ird.c ird.h A1_base.sprut A1_syntax.sprut A1_value.sprut A1_test.sprut
sprut_CFLAGS = -I$(srcdir)/../AMMUNITION
sprut_LDFLAGS = -static ../AMMUNITION/libcocom.la
dist_doc_DATA = sprut.txt sprut.info sprut.html sprut-1.html sprut-2.html sprut-3.html sprut-4.html sprut-5.html\
	        sprut.pdf sprut.dvi sprut.ps
dist_man_MANS = sprut.1
include_HEADERS = std_type.sprut std_mem.sprut std_bmem.sprut std_str.sprut std_pos.sprut
EXTRA_DIST =  yacc.y sprut-ird.h sprut-ird.c A_test.sprut A_syntax.sprut A_value.sprut A_base.sprut\
              ird.sprut\
	      sprut.tst README INSTALL sprut.sgml.in
ACLOCAL_AMFLAGS = -I m4

check : test

all-local:
dist-local:

# Delete all not needed for distribution.
distclean-local:
	rm -f sprut.sgml sprut.tex sprut.log sprut.aux sprut.toc sprut.tmp sprut.out\
	      A1_test.sprut A1_syntax.sprut A1_value.sprut A1_base.sprut

ird.c : $(srcdir)/sprut-ird.c
	rm -f $@
	cp $(srcdir)/sprut-ird.c $@
	chmod +w $@

ird.h : $(srcdir)/sprut-ird.h
	rm -f $@
	cp $(srcdir)/sprut-ird.h $@
	chmod +w $@

sprut-anal.$(OBJEXT) sprut-gen.$(OBJEXT) sprut-tab.$(OBJEXT) sprut-main.$(OBJEXT) sprut-yacc.$(OBJEXT): ird.h

yacc.c: $(srcdir)/yacc.y
	@echo must be because of error recovery \"conflicts: 2 shift/reduce\"
	$(YACC) $(srcdir)/yacc.y
	@echo
# This is necessary for MS-DOS YACC (BISON) and VMS POSIX YACC
	if test ! -f y.tab.c && test -f y_tab.c;then mv y_tab.c $@;\
	elif test ! -f y.tab.c && test -f ytab.c;then mv ytab.c $@;\
	else mv y.tab.c $@;fi

# B-tests and test of `sprut' built on base `ird.sprut'
test : sprut A-test A1-test B-tests sprut-boot
	@echo
	@echo "translation of internal representation of sprut itself"
	@echo ./sprut -macro -debug -access -set -copy $(srcdir)/ird.sprut
	@if ./sprut -macro -debug -access -set -copy $(srcdir)/ird.sprut; then\
		echo "+++ internal representation of sprut itself is ok";\
		rm -f ird.c ird.h;\
	else\
		echo "*** internal representation of sprut itself is bad";\
		echo "    see \"$(srcdir)/ird.sprut\"";\
		exit 1;\
	fi

# Test semantically correct description (A-test).
A-test:
	@echo
	@echo A_test is executed
	@echo C interface: non-flat structures and work with node fields
	@if ./sprut -all -debug $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: flat structures and non-flat work with node fields
	@if ./sprut -all -debug -flat-structure $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: non-flat structures and flat work with node fields
	@if ./sprut -all -debug -flat $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: flat structures and work with node fields
	@if ./sprut -all -debug -flat -flat-structure $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: Access and modification macros and functions
	@if ./sprut -all -debug -macro $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: Only access and modification macros
	@if ./sprut -all -debug -only-macro $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: Only access and modification functions
	@if ./sprut -all -debug $(srcdir)/A_test.sprut; then\
		if $(CC) A_test.c -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.c A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo
	@echo C++ interface: non-flat structures and work with node fields
	@if ./sprut -c++ -all -debug $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: flat structures and non-flat work with node fields
	@if ./sprut -c++ -all -debug -flat-structure $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: non-flat structures and flat work with node fields
	@if ./sprut -c++ -all -debug -flat $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: flat structures and work with node fields
	@if ./sprut -c++ -all -debug -flat -flat-structure $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: Access and modification macros and functions
	@if ./sprut -c++ -all -debug -macro $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: Only access and modification macros
	@if ./sprut -c++ -all -debug -only-macro $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: Only access and modification functions
	@if ./sprut -c++ -all -debug $(srcdir)/A_test.sprut; then\
		if $(CXX) A_test.cpp -o a.out; then\
			echo "+++ A_test is ok";\
			rm -f A_test.cpp A_test.h a.out;\
		else\
			echo "*** A_test is failed";\
			echo "    see \"$(srcdir)/A_test.sprut\", \"A_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A_test is failed -- see \"$(srcdir)/A_test.sprut\"";\
		exit 1;\
	fi
        
# The same as A_test but without %double
A1_base.sprut: $(srcdir)/A_base.sprut
	$(SED) "s/%double//g" $(srcdir)/A_base.sprut | $(SED) "s/A_/A1_/g" >$@
A1_syntax.sprut: $(srcdir)/A_syntax.sprut
	$(SED) "s/%double//g" $(srcdir)/A_syntax.sprut | $(SED) "s/A_/A1_/g" >$@
A1_value.sprut: $(srcdir)/A_value.sprut
	$(SED) "s/%double//g" $(srcdir)/A_value.sprut | $(SED) "s/A_/A1_/g" >$@
A1_test.sprut: $(srcdir)/A_test.sprut
	$(SED) "s/^%double.*$$//g" $(srcdir)/A_test.sprut | $(SED) "s/A_/A1_/g" >$@

# Test semantically correct description without double (A1-test).
A1-test: A1_base.sprut A1_syntax.sprut A1_value.sprut A1_test.sprut
	@echo
	@echo A1_test is executed
	@echo C interface: non-flat structures and work with node fields
	@if ./sprut -all -debug A1_test.sprut; then\
		if $(CC) A1_test.c -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: flat structures and non-flat work with node fields
	@if ./sprut -all -debug -flat-structure A1_test.sprut; then\
		if $(CC) A1_test.c -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: non-flat structures and flat work with node fields
	@if ./sprut -all -debug -flat A1_test.sprut; then\
		if $(CC) A1_test.c  -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: flat structures and work with node fields
	@if ./sprut -all -debug -flat -flat-structure A1_test.sprut; then\
		if $(CC) A1_test.c -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: Access and modification macros and functions
	@if ./sprut -all -debug -macro A1_test.sprut; then\
		if $(CC) A1_test.c -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: Only access and modification macros
	@if ./sprut -all -debug -only-macro A1_test.sprut; then\
		if $(CC) A1_test.c -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C interface: Only access and modification functions
	@if ./sprut -all -debug A1_test.sprut; then\
		if $(CC) A1_test.c -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.c A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.c\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo
	@echo C++ interface: non-flat structures and work with node fields
	@if ./sprut -c++ -all -debug A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: flat structures and non-flat work with node fields
	@if ./sprut -c++ -all -debug -flat-structure A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: non-flat structures and flat work with node fields
	@if ./sprut -c++ -all -debug -flat A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: flat structures and work with node fields
	@if ./sprut -c++ -all -debug -flat -flat-structure A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: Access and modification macros and functions
	@if ./sprut -c++ -all -debug -macro A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: Only access and modification macros
	@if ./sprut -c++ -all -debug -only-macro A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@echo C++ interface: Only access and modification functions
	@if ./sprut -c++ -all -debug A1_test.sprut; then\
		if $(CXX) A1_test.cpp -o a.out; then\
			echo "+++ A1_test is ok";\
			rm -f A1_test.cpp A1_test.h a.out;\
		else\
			echo "*** A1_test is failed";\
			echo "    see \"A1_test.sprut\", \"A1_test.cpp\"";\
			exit 1;\
		fi;\
	else\
		echo "*** A1_test is failed -- see \"A1_test.sprut\"";\
		exit 1;\
	fi
	@rm -r A1_base.sprut A1_syntax.sprut A1_value.sprut A1_test.sprut
        
# Test only semantically incorrect descriptions (B-tests).
B-tests:
	@echo
	@echo B_tests are executed
	$(SHELL) $(srcdir)/sprut.tst

# Make `sprut' by `ird.sprut'.
sprut-boot: sprut
	@echo
	./sprut -macro -debug -access -set -copy $(srcdir)/ird.sprut
	@echo Make \"sprut\" on the base of \"$(srcdir)/ird.sprut\"
	make sprut

# Creation of documentation

doc : 
	nroff -man $(srcdir)/sprut.1 | $(AWK) '{gsub (/.\b/,"");gsub (/.\200\230/,"\140");gsub (/.\200\231/,"\047");gsub (/\033\[[140]m/,"");gsub (/\033\[2[24]m/,"");print;}' > sprut.tmp
	$(AWK) '{if($$0 == "%%%")\
                     {while ((getline line < "sprut.tmp") > 0)\
                        print line;close("sprut.tmp")}\
                 else print;}' $(srcdir)/sprut.sgml.in > sprut.sgml
	rm -f sprut.tmp
	linuxdoc -B txt --papersize=letter -f sprut.sgml
	mv sprut.txt $(srcdir)
	linuxdoc -B html --papersize=letter sprut.sgml
	if test -f sprut-1.html;then\
  	  for i in sprut-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv sprut.html $(srcdir)/sprut.html
	linuxdoc -B info --papersize=letter sprut.sgml
	if test -f sprut.info;then\
	  for i in sprut.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	linuxdoc -B latex --papersize=letter sprut.sgml
	if test ! -f sprut.dvi\
           -o -f sprut.tex -a sprut.tex -nt sprut.dvi; then\
    	        latex sprut.tex; latex sprut.tex;\
        fi
	mv sprut.dvi $(srcdir)/sprut.dvi
	dvips -o $(srcdir)/sprut.ps $(srcdir)/sprut.dvi
	ps2pdf $(srcdir)/sprut.ps $(srcdir)/sprut.pdf
