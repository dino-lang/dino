# Cygwin can not find dll without changing PATH
PATH := ${PATH}:../../AMMUNITION/.libs

lib_LTLIBRARIES = d_mpi.la d_ieee.la d_ipcerr.la d_socket.la
CC = @CC@
DINO_MODULE_VERSION=@DINO_MODULE_VERSION@
d_mpi_la_SOURCES = d_mpi.c
d_mpi_la_CFLAGS = -shared @DEFS@ -I. -I.. -I$(srcdir) -I$(srcdir)/../../AMMUNITION
d_mpi_la_LDFLAGS = -shared -module @DINO_MOD_LDFLAGS@ ../../AMMUNITION/libcocom.la -release $(DINO_MODULE_VERSION)
d_ieee_la_SOURCES = d_ieee.c
d_ieee_la_CFLAGS = -shared @DEFS@ -DIEEE_QUAD -I. -I.. -I$(srcdir) -I$(srcdir)/../../AMMUNITION
d_ieee_la_LDFLAGS = -shared -module @DINO_MOD_LDFLAGS@ ../../AMMUNITION/libcocom.la -release $(DINO_MODULE_VERSION)
d_ipcerr_la_SOURCES = d_ipcerr.c
d_ipcerr_la_CFLAGS = -shared @DEFS@ -I. -I.. -I$(srcdir)
d_ipcerr_la_LDFLAGS = -shared -module @DINO_MOD_LDFLAGS@ -release $(DINO_MODULE_VERSION)
d_socket_la_SOURCES = d_socket.c
d_socket_la_CFLAGS = -shared @DEFS@ -I. -I.. -I$(srcdir)
d_socket_la_LDFLAGS = -shared -module @DINO_MOD_LDFLAGS@ -release $(DINO_MODULE_VERSION)
include_HEADERS = mpi.d ieee.d ipcerr.d socket.d
ACLOCAL_AMFLAGS = -I m4
