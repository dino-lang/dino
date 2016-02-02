#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Dino], 0.97, [vmakarov@gcc.gnu.org])
LT_INIT
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([d_config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([d_main.c])
AM_MAINTAINER_MODE

DINO_MOD_LDFLAGS=
system=`uname`.`uname -r`
case $system in
	Linux*)
	  DINO_LIB_CFLAGS='-static'
          DINO_LIB_LDFLAGS='-static'
	  DINO_MODULE_SUFFIX='so'
	  ;;
	Darwin*)
	  DINO_LIB_CFLAGS='-shared'
          DINO_LIB_LDFLAGS='-shared ../AMMUNITION/libcocom.la ../ONIGURUMA/libonig.la'
	  CFLAGS="`echo $CFLAGS` -I/usr/include -I/usr/local/include -L/usr/lib -L/usr/local/lib"
	  DINO_MODULE_SUFFIX='so'
          ;;
	CYGWIN*)
	  DINO_LIB_CFLAGS='-shared'
          DINO_LIB_LDFLAGS='-shared ../AMMUNITION/libcocom.la ../ONIGURUMA/libonig.la -no-undefined'
	  DINO_MOD_LDFLAGS='libdino.la -no-undefined'
	  CFLAGS="`echo $CFLAGS` -I/usr/include -I/usr/local/include -L/usr/lib -L/usr/local/lib"
	  DINO_MODULE_SUFFIX='dll'
          ;;
	*)
	  DINO_LIB_CFLAGS=-static
          DINO_LIB_LDFLAGS=-static 
	  DINO_MODULE_SUFFIX='so'
	  ;;
esac

AC_SUBST(DINO_LIB_CFLAGS)
AC_SUBST(DINO_LIB_LDFLAGS)
AC_SUBST(DINO_MOD_LDFLAGS)
AC_SUBST(DINO_MODULE_SUFFIX)

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_YACC
AC_PROG_LEX
AC_PROG_SED
AC_PROG_EGREP

# Checks for libraries.
AC_CHECK_LIB(gmp, __gmpz_init, found=yes,found=no)
if test $found = yes; then
  AC_CHECK_LIB(gmp, __gmpz_init)
else
  echo +++DINO requires gmp library -- Good bye.
  exit 1
fi
AC_CHECK_LIB(dl, dlopen)

AC_CHECK_LIB(iconv, iconv_open, found=yes,found=no)
if test $found = no; then
  AC_CHECK_LIB(iconv, libiconv_open, found=yes,found=no)
fi
if test $found = yes; then
  	  LDFLAGS="$LDLAGS -liconv"
fi

# Checks for header files.
AC_CHECK_HEADERS([dlfcn.h gmp.h iconv.h arpa/inet.h netdb.h netinet/in.h sys/socket.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_SIZE_T
AC_CHECK_SIZEOF(size_t)
AC_CHECK_SIZEOF(char *)
AC_CHECK_TYPES([ptrdiff_t])
AC_TYPE_SIGNAL

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_GETGROUPS
AC_FUNC_MALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([atexit getcwd gethostbyaddr gethostbyname gethostname inet_ntoa memmove memset\
                mkdir pow rmdir socket sqrt strerror strtoull strtod setitimer dlopen\
		sigaction sigemptyset getc_unlocked random])

AC_ARG_ENABLE([debug],
[  --enable-debug    Turn on debugging],
[case "${enableval}" in
      yes) dino_debug=true ;;
       no)  dino_debug= ;;
        *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
      esac],[dino_debug=])

AC_SUBST(START_SPRUT_OPTIONS)
if test x$dino_debug = x; then
  c_debug_define=-DNDEBUG
  START_SPRUT_OPTIONS=-only-macro
else
  my_save_cflags="$CFLAGS"
  CFLAGS=-g3
  AC_MSG_CHECKING([whether CC supports -g3])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
      [AC_MSG_RESULT([yes])]
      [g3=yes],
      [AC_MSG_RESULT([no])]
  )
  CFLAGS="$my_save_cflags"

  if test "$g3" = yes; then
    CFLAGS="`echo $CFLAGS|sed s/-O2//` -O0 -g3"
    CXXFLAGS="`echo $CXXFLAGS|sed s/-O2//` -O0 -g3"
  else
    CFLAGS="`echo $CFLAGS|sed s/-O2//` -O0 -g"
    CXXFLAGS="`echo $CXXFLAGS|sed s/-O2//` -O0 -g"
  fi
  c_debug_define=
  START_SPRUT_OPTIONS=-debug
fi

CFLAGS="$CFLAGS $c_debug_define"
CXXFLAGS="$CXXFLAGS $c_debug_define"

if test "$GCC" = yes && test x$dino_debug = x; then
  my_save_cflags="$CFLAGS"
  CFLAGS=-Ofast
  AC_MSG_CHECKING([whether CC supports -Ofast])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
      [AC_MSG_RESULT([yes])]
      [fast=yes],
      [AC_MSG_RESULT([no])]
  )
  CFLAGS="$my_save_cflags"
  if test "$fast" = yes; then
    CFLAGS="`echo $CFLAGS|sed s/-O2//` -Ofast"
    CXXFLAGS="`echo $CXXFLAGS|sed s/-O2//` -Ofast"
  else
    CFLAGS="`echo $CFLAGS|sed s/-O2//` -O3"
    CXXFLAGS="`echo $CXXFLAGS|sed s/-O2//` -O3"
  fi

  my_save_cflags="$CFLAGS"
  CFLAGS=-flto
  AC_MSG_CHECKING([whether CC supports -flto])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
      [AC_MSG_RESULT([yes])]
      [lto=yes],
      [AC_MSG_RESULT([no])]
  )

  CFLAGS="$my_save_cflags"

  fatlto=no
  if test "$lto" = yes; then
      CFLAGS="-flto -ffat-lto-objects"
      AC_MSG_CHECKING([whether CC supports -ffat-lto-objects])
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
            [AC_MSG_RESULT([yes])]
	    [fatlto=yes],
	    [AC_MSG_RESULT([no])]
      )
      CFLAGS="$my_save_cflags"
  fi
  
  if test "$lto" = yes; then
    CFLAGS="`echo $CFLAGS` -flto"
    CXXFLAGS="`echo $CXXFLAGS` -flto"
    if test "$fatlto" = yes; then
        CFLAGS="`echo $CFLAGS` -ffat-lto-objects"
	CXXFLAGS="`echo $CXXFLAGS` -ffat-lto-objects"
    fi
  fi

  system=`uname -m`
  case $system in
	i386*|i486*|i586*|i686*)
  	  CFLAGS="$CFLAGS -fomit-frame-pointer"
  	  CXXFLAGS="$CXXFLAGS -fomit-frame-pointer"
          ;;
	*)
	  ;;
  esac
fi

AC_CONFIG_FILES([Makefile])

output_files="dino.tst"

AC_OUTPUT($output_files)
