dnl Libvoikko: Finnish spellchecker and hyphenator library
dnl Copyright (C) 2006 - 2011 Harri Pitkänen <hatapitk@iki.fi>
dnl
dnl This program is free software; you can redistribute it and/or
dnl modify it under the terms of the GNU General Public License
dnl as published by the Free Software Foundation; either version 2
dnl of the License, or (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


dnl General options for autoconf
AC_PREREQ(2.60)
AC_INIT([libvoikko],[3.3],[hatapitk@iki.fi])
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
	[enable HFST morphology backend (EXPERIMENTAL!)]),
	[hfst=${enableval}], [hfst=no])
if test x$hfst = xyes; then
	AC_PROVIDE_IFELSE([PKG_CHECK_MODULES)], [
		PKG_CHECK_MODULES([HFSTOSPELL], [hfstospell])
		AC_DEFINE(HAVE_HFST, 1)
		CXXFLAGS="$CXXFLAGS $HFSTOSPELL_CFLAGS"
	], [
		AC_ERROR([You need to have pkg-config installed to be able to enable HFST])
	])
fi
AM_CONDITIONAL([HAVE_HFST], [test x$hfst = xyes])

dnl Lttoolbox support
AH_TEMPLATE(HAVE_LTTOOLBOX, [Have Lttoolbox])
AC_ARG_ENABLE(lttoolbox, AC_HELP_STRING([--enable-lttoolbox],
	[enable Lttoolbox morphology backend (EXPERIMENTAL!)]),
	[lttoolbox=${enableval}], [lttoolbox=no])
if test x$lttoolbox = xyes; then
	AC_PROVIDE_IFELSE([PKG_CHECK_MODULES)], [
		PKG_CHECK_MODULES([LTTOOLBOX], [lttoolbox-3.1 >= 3.1.0])
		AC_DEFINE(HAVE_LTTOOLBOX, 1)
		CXXFLAGS="$CXXFLAGS $LTTOOLBOX_CFLAGS"
	], [
		AC_ERROR([You need to have pkg-config installed to be able to enable Lttoolbox])
	])
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
AC_CHECK_FUNCS([setlocale])
AC_CHECK_FUNCS([mbrlen])
AC_CHECK_FUNCS([memset])
AC_CHECK_FUNCS([strchr])
AC_CHECK_FUNCS([getpwuid_r])

dnl Default compiler settings
CXXFLAGS="$CXXFLAGS -Wall -Werror -pedantic"

AC_CONFIG_FILES([Makefile src/Makefile src/tools/Makefile java/Makefile msvc/Makefile test/Makefile src/libvoikko.pc])
AC_OUTPUT
