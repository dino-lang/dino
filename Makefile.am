SUBDIRS = AMMUNITION SPRUT MSTA SHILKA ONIGURUMA DINO
dist_doc_DATA = cocom.txt cocom.ps cocom.pdf cocom.dvi cocom.info\
	        cocom.html
EXTRA_DIST = README.md Dino.jpg Dino_Flow.png INSTALL copyright COPYING COPYING.LIB cocom.sgml.in 

distclean-local:
	rm -f cocom.sgml cocom.tex cocom.log cocom.aux cocom.toc cocom.tmp cocom.out

doc: cocom-doc
	for i in AMMUNITION SPRUT MSTA SHILKA DINO NONA OKA;do (cd $$i && $(MAKE) doc);done

cocom-doc:
	cp $(srcdir)/cocom.sgml.in cocom.sgml
	linuxdoc -B txt --papersize=letter -f cocom.sgml
	mv cocom.txt $(srcdir)
	linuxdoc -B html --papersize=letter cocom.sgml
	if test -f cocom-1.html;then\
  	  for i in cocom-*.html;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	mv cocom.html $(srcdir)/cocom.html
	linuxdoc -B info --papersize=letter cocom.sgml
	if test -f cocom.info;then\
	  for i in cocom.info*;do\
   	    mv $$i $(srcdir)/$$i;\
	  done;\
        fi
	linuxdoc -B latex --papersize=letter cocom.sgml
	if test ! -f cocom.dvi\
           -o -f cocom.tex -a cocom.tex -nt cocom.dvi; then\
    	        latex cocom.tex; latex cocom.tex;\
        fi
	mv cocom.dvi $(srcdir)/cocom.dvi
	dvips -o $(srcdir)/cocom.ps $(srcdir)/cocom.dvi
	ps2pdf $(srcdir)/cocom.ps $(srcdir)/cocom.pdf
