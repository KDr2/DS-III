#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([KDI], [0.1], [killy.draw@gmail.com])
AM_INIT_AUTOMAKE([foreign -Wall -Werror])
AC_CONFIG_SRCDIR([src/base64.c])
AC_CONFIG_HEADERS([config.h])

AM_PROG_AR

LT_INIT


# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_LN_S
AC_PROG_RANLIB

# Checks for header files.
AC_CHECK_HEADERS([limits.h stddef.h stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memchr])

AC_ARG_ENABLE(test,
  AS_HELP_STRING( [--enable-test],
                  [compiling test code(yes|no, default=yes)]),
  [case "${enableval}" in
   yes) enable_test=yes ;;
   no)  enable_test=no ;;
   *)   AC_MSG_ERROR(bad value ${enableval} for --enable-test) ;;
   esac],
  [enable_test=yes])

AM_CONDITIONAL([BUILD_TEST],[test ${enable_test} = yes])

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 test/clibs/Makefile])
AC_OUTPUT
