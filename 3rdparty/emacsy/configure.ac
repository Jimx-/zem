# Emacsy --- An embeddable Emacs-like library using GNU Guile.
# Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
# Copyright (C) 2019 Jan Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Emacsy.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([Emacsy],
        [m4_esyscmd([build-aux/git-version-gen .tarball-version])],
        [guile-user@gnu.org],
        [emacsy])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall foreign -Wno-portability parallel-tests subdir-objects])
AM_SILENT_RULES([yes])

AC_PROG_CC_C99
AC_PROG_CC_C_O
AC_PROG_LIBTOOL

AC_PATH_PROG([BASH], [bash])
AC_SUBST([BASH])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

#GUILE_SITE_DIR
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

GUILE_FLAGS
AC_ARG_VAR([GUILD], [guild (Guile compiler) command])
AS_IF([test "x$GUILD" = "x"],
  [PKG_CHECK_VAR([GUILD], [guile-$GUILE_EFFECTIVE_VERSION], [guild], [],
    [AC_MSG_ERROR(m4_normalize([
      'guild' binary not found; please check your Guile installation.]))])])
AC_PATH_PROGS([guile_snarf], [guile-snarf guile-snarf$GUILE_EFFECTIVE_VERSION])
AC_SUBST(guile_snarf)

GUILE_MODULE_AVAILABLE([have_guile_lib], [(debugging assert)])
if test "x$have_guile_lib" != "xyes"; then
  AC_MSG_ERROR([guile-lib is missing; please install it.])
fi

AC_HEADER_STDC

AC_CHECK_PROG([HAVE_PDFLATEX], [pdflatex], [yes], [no])
AM_CONDITIONAL([HAVE_PDFLATEX], [test x$HAVE_PDFLATEX = xyes])

dnl '--with-examples=no' to disable building examples
AC_ARG_WITH([examples],
  AS_HELP_STRING([--without-examples], [Ignore and don't build examples]))

AS_IF([test x$with_examples != xno],
  [BUILD_EXAMPLES=yes],
  [BUILD_EXAMPLES=no])

PKG_CHECK_MODULES([FREEGLUT], [freeglut >= 3.0], [HAVE_FREEGLUT=yes], [HAVE_FREEGLUT=no])
PKG_CHECK_MODULES([GL], [gl >= 18.0.0], [HAVE_GL=yes], [HAVE_GL=no])
PKG_CHECK_MODULES([GLU], [glu >= 9.0.0], [HAVE_GLU=yes], [HAVE_GLU=no])
AM_CONDITIONAL([HELLO_EMACSY], [test x$BUILD_EXAMPLES$HAVE_FREEGLUT$HAVE_GL$HAVE_GLU = xyesyesyesyes])

PKG_CHECK_MODULES([WEBKIT], [webkit2gtk-4.0], [HAVE_WEBKIT=yes], [HAVE_WEBKIT=no])
AM_CONDITIONAL([EMACSY_WEBKIT_GTK], [test x$BUILD_EXAMPLES$HAVE_WEBKIT = xyesyes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([emacsy/emacsy.pc])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
                [chmod +x pre-inst-env])
AC_OUTPUT
