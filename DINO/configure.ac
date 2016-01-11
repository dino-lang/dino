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
		sigaction sigemptyset getc_unlocked])

AC_SUBST(START_SPRUT_OPTIONS)
AC_SUBST(C_DEBUG_DEFINE)
if test x$cocom_debug = x -a x$dino_debug = x; then
  C_DEBUG_DEFINE=-DNDEBUG
  START_SPRUT_OPTIONS=-only-macro
else
  CFLAGS=-g3
  AC_MSG_CHECKING([whether CC supports -g3])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
      [AC_MSG_RESULT([yes])]
      [g3=yes],
      [AC_MSG_RESULT([no])]
  )

  if test "$g3" = yes; then
    CFLAGS="`echo $CFLAGS|sed s/-O2//` -O0 -g3"
    CXXFLAGS="`echo $CXXFLAGS|sed s/-O2//` -O0 -g3"
  else
    CFLAGS="`echo $CFLAGS|sed s/-O2//` -O0 -g"
    CXXFLAGS="`echo $CXXFLAGS|sed s/-O2//` -O0 -g"
  fi
  C_DEBUG_DEFINE=
  START_SPRUT_OPTIONS=-debug
fi

system=`uname`.`uname -r`
case $system in
	Linux*)
          DINO_LINK_OPTION=-static
          ;;
	Darwin*)
          DINO_LINK_OPTION=-shared
          ;;
	*)
          DINO_LINK_OPTION=
	  ;;
esac

AC_SUBST(DINO_LINK_OPTION)

if test "$GCC" = yes && test x$cocom_debug = x -a x$dino_debug = x; then
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
