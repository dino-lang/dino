Summary: COCOM (Russian Armoury)
Name: cocom
Version: 0.995
Release: 1
Source0: %{name}-%{version}.tar.gz
License: GPL and LGPL
Group: Development/Tools
BuildRoot: %{_builddir}/%{name}-root

%description
COCOM tool set oriented towards the creation of compilers,
cross-compilers, interpreters, and other language processors.  And
high level scripting dynamic-typed language DINO.  DINO is oriented on
the same domain of applications as famous scripting languages perl,
tcl, python.

%prep
%setup -q

%build
./configure --prefix=/usr
make

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT

%define dino_version 0.54

%files
%defattr(0644,root,root) 
%attr(0555,root,root) /usr/bin/dino
%attr(0555,root,root) /usr/bin/msta
%attr(0555,root,root) /usr/bin/nona
%attr(0555,root,root) /usr/bin/oka
%attr(0555,root,root) /usr/bin/shilka
%attr(0555,root,root) /usr/bin/sprut
/usr/lib/libcocom.a
/usr/lib/libcocom++.a
/usr/lib/std_bmem.sprut
/usr/lib/std_mem.sprut
/usr/lib/std_pos.sprut
/usr/lib/std_str.sprut
/usr/lib/std_type.sprut
/usr/lib/dino-%{dino_version}/ieee.d
/usr/lib/dino-%{dino_version}/ieee.so
/usr/lib/dino-%{dino_version}/libdino.a
/usr/lib/dino-%{dino_version}/mpi.d
/usr/lib/dino-%{dino_version}/mpi.so
/usr/lib/dino-%{dino_version}/ipcerr.d
/usr/lib/dino-%{dino_version}/ipcerr.so
/usr/lib/dino-%{dino_version}/socket.d
/usr/lib/dino-%{dino_version}/socket.so
/usr/include/IEEE.h
/usr/include/allocate.h
/usr/include/arithm.h
/usr/include/bits.h
/usr/include/cocom-config.h
/usr/include/commline.h
/usr/include/earley.h
/usr/include/errors.h
/usr/include/hashtab.h
/usr/include/objstack.h
/usr/include/position.h
/usr/include/ticker.h
/usr/include/vlobject.h
/usr/include/dino-%{dino_version}/d_config.h
/usr/include/dino-%{dino_version}/d_extern.h
/usr/include/dino-%{dino_version}/d_types.h
%doc /usr/man/man1/dino.1*
%doc /usr/man/man1/msta.1*
%doc /usr/man/man1/nona.1*
%doc /usr/man/man1/oka.1*
%doc /usr/man/man1/shilka.1*
%doc /usr/man/man1/sprut.1*
%doc CHANGES INSTALL README cocom.txt cocom.dvi cocom.ps cocom.info cocom.html
%doc AMMUNITION/ammunition.txt AMMUNITION/ammunition.ps
%doc AMMUNITION/ammunition.dvi AMMUNITION/ammunition.info
%doc AMMUNITION/ammunition.info-1 AMMUNITION/ammunition.info-2
%doc AMMUNITION/ammunition.info-3
%doc AMMUNITION/ammunition.html AMMUNITION/ammunition-1.html
%doc AMMUNITION/ammunition-2.html AMMUNITION/ammunition-3.html
%doc AMMUNITION/ammunition-4.html AMMUNITION/ammunition-5.html
%doc AMMUNITION/ammunition-6.html AMMUNITION/ammunition-7.html
%doc AMMUNITION/ammunition-8.html AMMUNITION/ammunition-9.html
%doc AMMUNITION/ammunition-10.html AMMUNITION/ammunition-11.html
%doc AMMUNITION/ammunition-12.html AMMUNITION/ammunition-13.html
%doc AMMUNITION/ammunition++.txt AMMUNITION/ammunition++.ps
%doc AMMUNITION/ammunition++.dvi
%doc AMMUNITION/ammunition++.info AMMUNITION/ammunition++.info-1
%doc AMMUNITION/ammunition++.info-2 AMMUNITION/ammunition++.info-3
%doc AMMUNITION/ammunition++.html AMMUNITION/ammunition++-1.html
%doc AMMUNITION/ammunition++-2.html AMMUNITION/ammunition++-3.html
%doc AMMUNITION/ammunition++-4.html AMMUNITION/ammunition++-5.html
%doc AMMUNITION/ammunition++-6.html AMMUNITION/ammunition++-7.html
%doc AMMUNITION/ammunition++-8.html AMMUNITION/ammunition++-9.html
%doc AMMUNITION/ammunition++-10.html AMMUNITION/ammunition++-11.html
%doc AMMUNITION/ammunition++-12.html AMMUNITION/ammunition++-13.html
%doc DINO/dino.txt DINO/dino.ps DINO/dino.dvi
%doc DINO/dino.info DINO/dino.info-1 DINO/dino.info-2
%doc DINO/dino.info-3 DINO/dino.info-4
%doc DINO/dino.html DINO/dino-1.html DINO/dino-2.html
%doc DINO/dino-3.html DINO/dino-4.html DINO/dino-5.html DINO/dino-6.html
%doc DINO/dino-7.html DINO/dino-8.html DINO/dino-9.html DINO/dino-10.html
%doc MSTA/msta.txt MSTA/msta.ps MSTA/msta.dvi
%doc MSTA/msta.info MSTA/msta.info-1 MSTA/msta.info-2
%doc MSTA/msta.html MSTA/msta-1.html MSTA/msta-2.html
%doc MSTA/msta-3.html MSTA/msta-4.html MSTA/msta-5.html MSTA/msta-6.html
%doc MSTA/msta-7.html MSTA/msta-8.html MSTA/msta-9.html
%doc NONA/nona.txt NONA/nona.ps NONA/nona.dvi NONA/nona.info
%doc NONA/nona.html NONA/nona-1.html NONA/nona-2.html NONA/nona-3.html
%doc NONA/nona-4.html NONA/nona-5.html
%doc OKA/oka.txt OKA/oka.ps OKA/oka.dvi OKA/oka.info
%doc OKA/oka.html OKA/oka-1.html OKA/oka-2.html OKA/oka-3.html OKA/oka-4.html
%doc OKA/oka-5.html OKA/oka-6.html OKA/oka-7.html OKA/oka-8.html OKA/oka-9.html
%doc SHILKA/shilka.txt SHILKA/shilka.ps SHILKA/shilka.dvi SHILKA/shilka.info
%doc SHILKA/shilka.html SHILKA/shilka-1.html SHILKA/shilka-2.html
%doc SHILKA/shilka-3.html SHILKA/shilka-4.html SHILKA/shilka-5.html
%doc SHILKA/shilka-6.html SHILKA/shilka-7.html SHILKA/shilka-8.html
%doc SPRUT/sprut.txt SPRUT/sprut.ps SPRUT/sprut.dvi
%doc SPRUT/sprut.info SPRUT/sprut.info-1 SPRUT/sprut.info-2
%doc SPRUT/sprut.html SPRUT/sprut-1.html SPRUT/sprut-2.html SPRUT/sprut-3.html
%doc SPRUT/sprut-4.html SPRUT/sprut-5.html
