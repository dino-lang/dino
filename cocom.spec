Summary: COCOM
Name: cocom
Version: 0.997
Release: 1
Source0: %{name}-%{version}.tar.gz
License: GPL and LGPL
Group: Development/Tools
BuildRoot: %{_builddir}/%{name}-root
BuildRequires: gmp-devel
Requires: gmp

%description
COCOM tool set oriented towards the creation of compilers,
cross-compilers, interpreters, and other language processors.  And
high level scripting dynamic-typed language DINO.  DINO is oriented on
the same domain of applications as popular scripting languages perl,
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

%define dino_version 0.91

%files
%defattr(0644,root,root) 
%attr(0555,root,root) /usr/bin/dino
%attr(0555,root,root) /usr/bin/dino-%{dino_version}
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
/usr/lib/dino-%{dino_version}/gmp.d
/usr/lib/dino-%{dino_version}/gmp.so
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
%doc /usr/share/man/man1/dino.1*
%doc /usr/share/man/man1/msta.1*
%doc /usr/share/man/man1/nona.1*
%doc /usr/share/man/man1/oka.1*
%doc /usr/share/man/man1/shilka.1*
%doc /usr/share/man/man1/sprut.1*
%doc /usr/share/doc/cocom/CHANGES
%doc /usr/share/doc/cocom/README
%doc /usr/share/doc/cocom/copyright
%doc /usr/share/doc/cocom/cocom.txt
%doc /usr/share/doc/cocom/cocom.dvi
%doc /usr/share/doc/cocom/cocom.ps
%doc /usr/share/doc/cocom/cocom.pdf
%doc /usr/share/doc/cocom/cocom.info
%doc /usr/share/doc/cocom/cocom.html
%doc /usr/share/doc/cocom/ammunition.txt
%doc /usr/share/doc/cocom/ammunition.ps
%doc /usr/share/doc/cocom/ammunition.pdf
%doc /usr/share/doc/cocom/ammunition.dvi
%doc /usr/share/doc/cocom/ammunition.info
%doc /usr/share/doc/cocom/ammunition.html
%doc /usr/share/doc/cocom/ammunition-1.html
%doc /usr/share/doc/cocom/ammunition-2.html
%doc /usr/share/doc/cocom/ammunition-3.html
%doc /usr/share/doc/cocom/ammunition-4.html
%doc /usr/share/doc/cocom/ammunition-5.html
%doc /usr/share/doc/cocom/ammunition-6.html
%doc /usr/share/doc/cocom/ammunition-7.html
%doc /usr/share/doc/cocom/ammunition-8.html
%doc /usr/share/doc/cocom/ammunition-9.html
%doc /usr/share/doc/cocom/ammunition-10.html
%doc /usr/share/doc/cocom/ammunition-11.html
%doc /usr/share/doc/cocom/ammunition-12.html
%doc /usr/share/doc/cocom/ammunition-13.html
%doc /usr/share/doc/cocom/ammunition++.txt
%doc /usr/share/doc/cocom/ammunition++.ps
%doc /usr/share/doc/cocom/ammunition++.pdf
%doc /usr/share/doc/cocom/ammunition++.dvi
%doc /usr/share/doc/cocom/ammunition++.info
%doc /usr/share/doc/cocom/ammunition++.html
%doc /usr/share/doc/cocom/ammunition++-1.html
%doc /usr/share/doc/cocom/ammunition++-2.html
%doc /usr/share/doc/cocom/ammunition++-3.html
%doc /usr/share/doc/cocom/ammunition++-4.html
%doc /usr/share/doc/cocom/ammunition++-5.html
%doc /usr/share/doc/cocom/ammunition++-6.html
%doc /usr/share/doc/cocom/ammunition++-7.html
%doc /usr/share/doc/cocom/ammunition++-8.html
%doc /usr/share/doc/cocom/ammunition++-9.html
%doc /usr/share/doc/cocom/ammunition++-10.html
%doc /usr/share/doc/cocom/ammunition++-11.html
%doc /usr/share/doc/cocom/ammunition++-12.html
%doc /usr/share/doc/cocom/ammunition++-13.html
%doc /usr/share/doc/dino/README
%doc /usr/share/doc/dino/CHANGES
%doc /usr/share/doc/dino/copyright
%doc /usr/share/doc/dino/dino.txt
%doc /usr/share/doc/dino/dino.txt
%doc /usr/share/doc/dino/dino.ps
%doc /usr/share/doc/dino/dino.pdf
%doc /usr/share/doc/dino/dino.dvi
%doc /usr/share/doc/dino/dino.info
%doc /usr/share/doc/dino/dino.info-1
%doc /usr/share/doc/dino/dino.info-2
%doc /usr/share/doc/dino/dino.info-3
%doc /usr/share/doc/dino/dino.info-4
%doc /usr/share/doc/dino/dino.html
%doc /usr/share/doc/dino/dino-1.html
%doc /usr/share/doc/dino/dino-2.html
%doc /usr/share/doc/dino/dino-3.html
%doc /usr/share/doc/dino/dino-4.html
%doc /usr/share/doc/dino/dino-5.html
%doc /usr/share/doc/dino/dino-6.html
%doc /usr/share/doc/dino/dino-7.html
%doc /usr/share/doc/dino/dino-8.html
%doc /usr/share/doc/dino/dino-9.html
%doc /usr/share/doc/dino/dino-10.html
%doc /usr/share/doc/cocom/msta.txt
%doc /usr/share/doc/cocom/msta.ps
%doc /usr/share/doc/cocom/msta.pdf
%doc /usr/share/doc/cocom/msta.dvi
%doc /usr/share/doc/cocom/msta.info
%doc /usr/share/doc/cocom/msta.info-1
%doc /usr/share/doc/cocom/msta.info-2
%doc /usr/share/doc/cocom/msta.html
%doc /usr/share/doc/cocom/msta-1.html
%doc /usr/share/doc/cocom/msta-2.html
%doc /usr/share/doc/cocom/msta-3.html
%doc /usr/share/doc/cocom/msta-4.html
%doc /usr/share/doc/cocom/msta-5.html
%doc /usr/share/doc/cocom/msta-6.html
%doc /usr/share/doc/cocom/msta-7.html
%doc /usr/share/doc/cocom/msta-8.html
%doc /usr/share/doc/cocom/msta-9.html
%doc /usr/share/doc/cocom/nona.txt
%doc /usr/share/doc/cocom/nona.ps
%doc /usr/share/doc/cocom/nona.pdf
%doc /usr/share/doc/cocom/nona.dvi
%doc /usr/share/doc/cocom/nona.info
%doc /usr/share/doc/cocom/nona.html
%doc /usr/share/doc/cocom/nona-1.html
%doc /usr/share/doc/cocom/nona-2.html
%doc /usr/share/doc/cocom/nona-3.html
%doc /usr/share/doc/cocom/nona-4.html
%doc /usr/share/doc/cocom/nona-5.html
%doc /usr/share/doc/cocom/oka.txt
%doc /usr/share/doc/cocom/oka.ps
%doc /usr/share/doc/cocom/oka.pdf
%doc /usr/share/doc/cocom/oka.dvi
%doc /usr/share/doc/cocom/oka.info
%doc /usr/share/doc/cocom/oka.html
%doc /usr/share/doc/cocom/oka-1.html
%doc /usr/share/doc/cocom/oka-2.html
%doc /usr/share/doc/cocom/oka-3.html
%doc /usr/share/doc/cocom/oka-4.html
%doc /usr/share/doc/cocom/oka-5.html
%doc /usr/share/doc/cocom/oka-6.html
%doc /usr/share/doc/cocom/oka-7.html
%doc /usr/share/doc/cocom/oka-8.html
%doc /usr/share/doc/cocom/oka-9.html
%doc /usr/share/doc/cocom/shilka.txt
%doc /usr/share/doc/cocom/shilka.ps
%doc /usr/share/doc/cocom/shilka.pdf
%doc /usr/share/doc/cocom/shilka.dvi
%doc /usr/share/doc/cocom/shilka.info
%doc /usr/share/doc/cocom/shilka.html
%doc /usr/share/doc/cocom/shilka-1.html
%doc /usr/share/doc/cocom/shilka-2.html
%doc /usr/share/doc/cocom/shilka-3.html
%doc /usr/share/doc/cocom/shilka-4.html
%doc /usr/share/doc/cocom/shilka-5.html
%doc /usr/share/doc/cocom/shilka-6.html
%doc /usr/share/doc/cocom/shilka-7.html
%doc /usr/share/doc/cocom/shilka-8.html
%doc /usr/share/doc/cocom/sprut.txt
%doc /usr/share/doc/cocom/sprut.ps
%doc /usr/share/doc/cocom/sprut.pdf
%doc /usr/share/doc/cocom/sprut.dvi
%doc /usr/share/doc/cocom/sprut.info
%doc /usr/share/doc/cocom/sprut.info-1
%doc /usr/share/doc/cocom/sprut.info-2
%doc /usr/share/doc/cocom/sprut.html
%doc /usr/share/doc/cocom/sprut-1.html
%doc /usr/share/doc/cocom/sprut-2.html
%doc /usr/share/doc/cocom/sprut-3.html
%doc /usr/share/doc/cocom/sprut-4.html
%doc /usr/share/doc/cocom/sprut-5.html
