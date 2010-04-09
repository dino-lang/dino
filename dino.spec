Summary: Interpreter of the language DINO
Name: dino
Version: 0.55
Release: 1
Source0: %{name}-%{version}.tar.gz
License: GPL
Group: Development/Tools
BuildRoot: %{_builddir}/%{name}-root
BuildRequires: gmp-devel
Requires: gmp

%description
DINO is oriented on the same domain of applications as famous
scripting languages perl, tcl, python.

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

%files
%defattr(0644,root,root) 
%attr(0555,root,root) /usr/bin/dino
%attr(0555,root,root) /usr/bin/dino-%{version}
/usr/lib/dino-%{version}/ieee.d
/usr/lib/dino-%{version}/ieee.so
/usr/lib/dino-%{version}/libdino.a
/usr/lib/dino-%{version}/mpi.d
/usr/lib/dino-%{version}/mpi.so
/usr/lib/dino-%{version}/ipcerr.d
/usr/lib/dino-%{version}/ipcerr.so
/usr/lib/dino-%{version}/socket.d
/usr/lib/dino-%{version}/socket.so
/usr/lib/dino-%{version}/gmp.d
/usr/lib/dino-%{version}/gmp.so
/usr/include/dino-%{version}/d_config.h
/usr/include/dino-%{version}/d_extern.h
/usr/include/dino-%{version}/d_types.h
%doc /usr/share/man/man1/dino.1*
%doc /usr/share/doc/dino/CHANGES
%doc /usr/share/doc/dino/README
%doc /usr/share/doc/dino/copyright
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
