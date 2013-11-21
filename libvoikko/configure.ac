# The contents of this file are subject to the Mozilla Public License Version 
# 1.1 (the "License"); you may not use this file except in compliance with 
# the License. You may obtain a copy of the License at 
# http://www.mozilla.org/MPL/
# 
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# for the specific language governing rights and limitations under the
# License.
# 
# The Original Code is Libvoikko: Library of natural language processing tools.
# The Initial Developer of the Original Code is Harri Pitkänen <hatapitk@iki.fi>.
# Portions created by the Initial Developer are Copyright (C) 2006 - 2011
# the Initial Developer. All Rights Reserved.
# 
# Alternatively, the contents of this file may be used under the terms of
# either the GNU General Public License Version 2 or later (the "GPL"), or
# the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
# in which case the provisions of the GPL or the LGPL are applicable instead
# of those above. If you wish to allow use of your version of this file only
# under the terms of either the GPL or the LGPL, and not to allow others to
# use your version of this file under the terms of the MPL, indicate your
# decision by deleting the provisions above and replace them with the notice
# and other provisions required by the GPL or the LGPL. If you do not delete
# the provisions above, a recipient may use your version of this file under
# the terms of any one of the MPL, the GPL or the LGPL.


dnl General options for autoconf
AC_PREREQ(2.60)
AC_INIT([libvoikko],[3.7],[hatapitk@iki.fi])
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE([1.10 foreign])
AC_CONFIG_MACRO_DIR([m4])

dnl Checks for programs
AC_PROG_CXX
AC_PROG_INSTALL

dnl Libtool (new and old)
AC_LIBTOOL_WIN32_DLL
ifdef([LT_INIT], [LT_INIT], [AC_PROG_LIBTOOL])

AM_MAINTAINER_MODE

dnl Compiler features
AC_LANG_CPLUSPLUS
AC_C_CONST

# FIXME: the following is wrong (it should be possible to change this when running make)
if test ${prefix} = "NONE"
then
	DICTIONARY_PATH=${ac_default_prefix}/lib/voikko
else
	DICTIONARY_PATH=${prefix}/lib/voikko
fi
AC_ARG_WITH(dictionary-path,
            AC_HELP_STRING([--with-dictionary-path=PATH],
	                   [Dictionary path]),
	    [DICTIONARY_PATH="$withval"])
AC_DEFINE_UNQUOTED(DICTIONARY_PATH, "$DICTIONARY_PATH", [Dictionary path])
AC_SUBST(DICTIONARY_PATH)

dnl Checks for headers
AC_CHECK_HEADERS([stddef.h])
AC_CHECK_HEADERS([stdlib.h])
AC_CHECK_HEADERS([langinfo.h])
AC_CHECK_HEADERS([locale.h])
AC_CHECK_HEADERS([wctype.h])
AC_CHECK_HEADERS([wchar.h],,[AC_MSG_ERROR([wchar.h not found])])

dnl Types
AC_TYPE_SIZE_T

dnl GCC visibility support
AH_TEMPLATE(GCC_VISIBILITY, [GCC visibility support])
tmp_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -Wall -Werror -fvisibility=hidden"
AC_MSG_CHECKING([whether $CC supports the visibility attribute])
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
[
  void __attribute__ ((visibility("default"))) test_default (void) {}
  void __attribute__ ((visibility("hidden"))) test_hidden (void) {}
 int main (int argc, char **argv) { test_default (); test_hidden (); return 0; }
])],
[
  AC_DEFINE(GCC_VISIBILITY, 1)
  CFLAGS="$CFLAGS -fvisibility=hidden"
  CXXFLAGS="$tmp_CXXFLAGS -fvisibility=hidden"
  AC_MSG_RESULT([yes])
],
[
  CXXFLAGS="$tmp_CXXFLAGS"
  AC_MSG_RESULT([no])
])

dnl Build tools
buildtools="no"
AC_ARG_ENABLE(buildtools, AC_HELP_STRING([--enable-buildtools],
        [enable voikkovfstc (EXPERIMENTAL!)]),
        [buildtools=${enableval}], [buildtools=no])
AM_CONDITIONAL([HAVE_BUILDTOOLS], [test x$buildtools = xyes])

dnl Test tools
testtools="yes"
AC_ARG_ENABLE(testtools, AC_HELP_STRING([--disable-testtools],
	[disable voikkospell, voikkohyphenate and voikkogc]),
	[testtools=${enableval}], [testtools=yes])
AM_CONDITIONAL([HAVE_TESTTOOLS], [test x$testtools = xyes])
	
dnl Malaga support
malaga="yes"
AH_TEMPLATE(HAVE_MALAGA, [Have MALAGA])
AC_ARG_ENABLE(malaga, AC_HELP_STRING([--disable-malaga],
	[disable Malaga morphology backend (EXPERIMENTAL!)]),
	[malaga=${enableval}], [malaga=yes])
if test x$malaga = xyes; then
	AC_DEFINE(HAVE_MALAGA, 1)
fi
AM_CONDITIONAL([HAVE_MALAGA], [test x$malaga = xyes])

dnl HFST support
AH_TEMPLATE(HAVE_HFST, [Have HFST])
AC_ARG_ENABLE(hfst, AC_HELP_STRING([--enable-hfst],
	[enable HFST morphology backend]),
	[hfst=${enableval}], [hfst=no])
dnl must be AS_IF for some aclocals to pick PKG_CHECK_MODULES somehow.
dnl AC_PROVIDE_IFELSE doesn't work on mac?
AS_IF([test x$hfst = xyes], [
      PKG_CHECK_MODULES([HFSTOSPELL], [hfstospell >= 0.2])
	  AC_DEFINE(HAVE_HFST, 1)
	  CXXFLAGS="$CXXFLAGS $HFSTOSPELL_CFLAGS"
	])
AM_CONDITIONAL([HAVE_HFST], [test x$hfst = xyes])

dnl VISLCG3 support
AH_TEMPLATE(HAVE_VISLCG3, [Have VISLCG3])
AC_ARG_ENABLE(vislcg3, AC_HELP_STRING([--enable-vislcg3],
	[enable vislcg3 grammar checking backend (EXPERIMENTAL!)]),
	[cg3=${enableval}], [cg3=no])
dnl must be AS_IF for some aclocals to pick PKG_CHECK_MODULES somehow.
dnl AC_PROVIDE_IFELSE doesn't work on mac?
AS_IF([test x$cg3 = xyes], [
      PKG_CHECK_MODULES([VISLCG3], [cg3-0.9 >= 0.9])
	  AC_DEFINE(HAVE_VISLCG3, 1)
	  CXXFLAGS="$CXXFLAGS $VISLCG3_CFLAGS"
	])
dnl this does not work as it should AC_CHECK_HEADER([tinyxml2.h], AC_DEFINE(HAVE_VISLCG3, 1), AC_DEFINE(HAVE_VISLCG3, 0))
AM_CONDITIONAL([HAVE_VISLCG3], [test x$cg3 = xyes])


dnl VFST support
AH_TEMPLATE(HAVE_VFST, [Have VFST])
AC_ARG_ENABLE(vfst, AC_HELP_STRING([--enable-vfst],
	[enable VFST morphology backend (EXPERIMENTAL!)]),
	[vfst=${enableval}], [vfst=no])
if test x$vfst = xyes; then
	AC_DEFINE(HAVE_VFST, 1)
fi
AM_CONDITIONAL([HAVE_VFST], [test x$vfst = xyes])

dnl Lttoolbox support
AH_TEMPLATE(HAVE_LTTOOLBOX, [Have Lttoolbox])
AC_ARG_ENABLE(lttoolbox, AC_HELP_STRING([--enable-lttoolbox],
	[enable Lttoolbox morphology backend (EXPERIMENTAL!)]),
	[lttoolbox=${enableval}], [lttoolbox=no])
if test x$lttoolbox = xyes; then
	PKG_CHECK_MODULES([LTTOOLBOX], [lttoolbox-3.2 >= 3.2.0])
	AC_DEFINE(HAVE_LTTOOLBOX, 1)
	CXXFLAGS="$CXXFLAGS $LTTOOLBOX_CFLAGS"
fi
AM_CONDITIONAL([HAVE_LTTOOLBOX], [test x$lttoolbox = xyes])

dnl External dictionary loading
external_dicts="yes"
AH_TEMPLATE(DISABLE_EXTDICTS, [Disable external dictionaries])
AC_ARG_ENABLE(external_dicts, AC_HELP_STRING([--disable-external-dicts],
	[disable loading of external dictionaries (use with experimental or patched builds)]),
	[external_dicts=${enableval}], [external_dicts=yes])
if test x$external_dicts = xno; then
	AC_DEFINE(DISABLE_EXTDICTS, 1)
fi


dnl Windows specific settings
if test x$build_os == xmingw32; then
  LIBLDFLAGSWIN="-Wl,--output-def,libvoikko-1.def"
fi
AC_SUBST(LIBLDFLAGSWIN)

ACX_PTHREAD([AM_CONDITIONAL([HAVE_PTHREAD],[true])], [AM_CONDITIONAL([HAVE_PTHREAD],[false])])

dnl Functions
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_CHECK_FUNCS([mmap])
AC_CHECK_FUNCS([setlocale])
AC_CHECK_FUNCS([mbrlen])
AC_CHECK_FUNCS([memset])
AC_CHECK_FUNCS([strchr])
AC_CHECK_FUNCS([getpwuid_r])

dnl Default compiler settings
CXXFLAGS="$CXXFLAGS -Wall -Werror -pedantic"

AC_CONFIG_FILES([Makefile src/Makefile src/tools/Makefile java/Makefile msvc/Makefile test/Makefile cs/Makefile cl/Makefile src/libvoikko.pc])
AC_OUTPUT
