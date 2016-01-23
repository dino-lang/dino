#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Nona], 0.97, [vmakarov@gcc.gnu.org])
LT_INIT
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([anal.c])
AM_MAINTAINER_MODE

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_YACC

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIGNAL

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE

AC_CONFIG_FILES([Makefile])

AC_ARG_ENABLE([debug],
[  --enable-debug    Turn on debugging],
[case "${enableval}" in
      yes) cocom_debug=true ;;
       no)  cocom_debug= ;;
        *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
      esac],[cocom_debug=])

AC_SUBST(START_SPRUT_OPTIONS)

if test x$cocom_debug = x; then
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
    CFLAGS='-O0 -g3'
    CXXFLAGS='-O0 -g3'
  else
    CFLAGS='-O0 -g'
    CXXFLAGS='-O0 -g'
  fi
  c_debug_define=
  START_SPRUT_OPTIONS=-debug
fi

CFLAGS="$CFLAGS $c_debug_define"
CXXFLAGS="$CXXFLAGS $c_debug_define"

AC_OUTPUT
