#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Ammunition], 0.97, [vmakarov@gcc.gnu.org])
LT_INIT
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([spset.h])
AM_MAINTAINER_MODE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memmove memset strerror])

AC_PROG_YACC
AC_PROG_LEX

AC_CONFIG_FILES([Makefile])

AC_SUBST(YAEPLIB)
AC_SUBST(YAEPDIR)
YAEPLIB=cocom
YAEPDIR=../AMMUNITION/.libs

system=`uname`.`uname -r`
case $system in
	CYGWIN*)
	  LIB_CFLAGS=-shared
          LIB_LDFLAGS=-shared -no-undefined
          ;;
	*)
	  LIB_CFLAGS=
          LIB_LDFLAGS=
	  ;;
esac

AC_SUBST(LIB_CFLAGS)
AC_SUBST(LIB_LDFLAGS)

AC_SUBST(C_DEBUG_DEFINE)
if test x$cocom_debug = x -a x$dino_debug = x; then
  C_DEBUG_DEFINE=-DNDEBUG
else
  CFLAGS=-g3
  AC_MSG_CHECKING([whether CC supports -g3])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
      [AC_MSG_RESULT([yes])]
      [g3=yes],
      [AC_MSG_RESULT([no])]
  )

  if test "$g3" = yes; then
    CFLAGS='-O0 -g3'
    CXXFLAGS='-O0 -g3'
  else
    CFLAGS='-O0 -g'
    CXXFLAGS='-O0 -g'
  fi
  C_DEBUG_DEFINE=
fi

output_files="allocate.tst vlobject.tst objstack.tst hashtab.tst\
              spset.tst cspset.tst commline.tst position.tst\
              errors.tst ticker.tst bits.tst arithm.tst IEEE.tst yaep.tst\
              allocate++.tst vlobject++.tst objstack++.tst hashtab++.tst\
              spset++.tst cspset++.tst commline++.tst position++.tst\
              errors++.tst ticker++.tst bits++.tst arithm++.tst IEEE++.tst\
              yaep++.tst compare_parsers.tst"

AC_OUTPUT($output_files)
