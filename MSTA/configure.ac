#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Msta], 0.97, [vmakarov@gcc.gnu.org])
LT_INIT
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([anal.c])

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_YACC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_SIZE_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([pow])

AC_TYPE_SIGNAL

AC_CONFIG_FILES([Makefile])

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
    CFLAGS='-O0 -g3'
    CXXFLAGS='-O0 -g3'
  else
    CFLAGS='-O0 -g'
    CXXFLAGS='-O0 -g'
  fi
  C_DEBUG_DEFINE=
  START_SPRUT_OPTIONS=-debug
fi

output_files="msta.tst quality.tst"
AC_OUTPUT($output_files)
