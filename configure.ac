#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([myguix],
  [m4_esyscmd([echo -n 0.1.0])],
  [bvits@riseup.net], [myguix],
  [https://github.com/b-vitamins/myguix])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

# Enable silent rules by default.
AM_SILENT_RULES([yes])

AC_CONFIG_SRCDIR([myguix/packages.scm])
AC_CONFIG_MACRO_DIR([m4])

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([GUILE_MODULE_AVAILABLE])
m4_pattern_forbid([^GUILE_P])
m4_pattern_allow([^GUILE_PKG_ERRORS])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
fi

dnl Guile 3.0.3 or later is required.
PKG_CHECK_MODULES([GUILE], [guile-3.0 >= 3.0.3])

dnl Get CFLAGS and LDFLAGS for libguile.
GUILE_FLAGS

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

dnl Check for required Guile modules that a channel might need.
dnl Most actual Guix modules will be available from the host Guix.

dnl Make sure we have a full-fledged Guile.
GUILE_MODULE_REQUIRED([srfi srfi-64])  dnl For tests

dnl Check if we can run tests (file name limits)
AC_MSG_CHECKING([whether file names can exceed 99 characters])
AC_CACHE_VAL([ac_cv_guix_test_root],
  [ac_cv_guix_test_root=test-tmp
   mkdir -p "$ac_cv_guix_test_root"
   if (touch "$ac_cv_guix_test_root/"`printf %100s | sed -e 's/ /x/g'` 2>/dev/null); then
     ac_cv_guix_test_root=yes
   else
     ac_cv_guix_test_root=no
   fi
   rm -rf test-tmp])
AC_MSG_RESULT([$ac_cv_guix_test_root])

AM_CONDITIONAL([CAN_RUN_TESTS], [test "x$ac_cv_guix_test_root" = "xyes"])

AC_PROG_SED

AC_MSG_CHECKING([if building from git])
if test -e .git; then
   in_git_p=yes
else
   in_git_p=no
fi
AC_MSG_RESULT([$in_git_p])
AM_CONDITIONAL([in_git_p], [test "x$in_git_p" = "xyes"])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([test-env:build-aux/test-env.in], [chmod +x test-env])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])

AC_OUTPUT
