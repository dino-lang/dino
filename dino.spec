Summary: Interpreter of the language DINO
Name: dino
Version: 0.55
Release: 1
Source0: %{name}-%{version}.tar.gz
License: GPL
Group: Development/Tools
BuildRoot: %{_builddir}/%{name}-root

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
/usr/lib/dino-%{version}/ieee.d
/usr/lib/dino-%{version}/ieee.so
/usr/lib/dino-%{version}/libdino.a
/usr/lib/dino-%{version}/mpi.d
/usr/lib/dino-%{version}/mpi.so
/usr/lib/dino-%{version}/ipcerr.d
/usr/lib/dino-%{version}/ipcerr.so
/usr/lib/dino-%{version}/socket.d
/usr/lib/dino-%{version}/socket.so
/usr/include/dino-%{version}/d_config.h
/usr/include/dino-%{version}/d_extern.h
/usr/include/dino-%{version}/d_types.h
%doc /usr/man/man1/dino.1*
%doc %attr(0555,root,root) CHANGES INSTALL README
%doc DINO/dino.txt DINO/dino.ps DINO/dino.dvi
%doc DINO/dino.info DINO/dino.info-1 DINO/dino.info-2
%doc DINO/dino.info-3 DINO/dino.info-4
%doc DINO/dino.html DINO/dino-1.html DINO/dino-2.html
%doc DINO/dino-3.html DINO/dino-4.html DINO/dino-5.html
%doc DINO/dino-6.html DINO/dino-7.html
%doc DINO/dino-8.html DINO/dino-9.html DINO/dino-10.html
