dnl Libvoikko: Finnish spellchecker and hyphenator library
dnl Copyright (C) 2006 Harri Pitkänen <hatapitk@iki.fi>
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
AC_INIT([libvoikko],[2.1],[hatapitk@iki.fi])
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE([1.10 foreign])

dnl Checks for programs
AC_PROG_CXX
AC_PROG_INSTALL
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
#AC_PROG_RANLIB

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
AC_CHECK_HEADERS([malaga.h],,[AC_MSG_ERROR([malaga.h not found])])

dnl Types
AC_TYPE_SIZE_T

dnl GCC visibility support
AH_TEMPLATE(GCC_VISIBILITY, [GCC visibility support])
tmp_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -Wall -Werror -fvisibility=hidden"
AC_MSG_CHECKING([whether $CC supports the visibility attribute])
AC_COMPILE_IFELSE(AC_LANG_SOURCE(
[
  void __attribute__ ((visibility("default"))) test_default (void) {}
  void __attribute__ ((visibility("hidden"))) test_hidden (void) {}
 int main (int argc, char **argv) { test_default (); test_hidden (); return 0; }
]),
[
  AC_DEFINE(GCC_VISIBILITY, 1)
  CFLAGS="$tmp_CFLAGS -fvisibility=hidden"
  CXXFLAGS="$CXXFLAGS -fvisibility=hidden"
  AC_MSG_RESULT([yes])
],
[
  CFLAGS="$tmp_CFLAGS"
  AC_MSG_RESULT([no])
])

dnl Windows specific settings
if test x$build_os == xmingw32; then
  LIBLDFLAGSWIN="-Wl,--output-def,libvoikko-1.def"
fi
AC_SUBST(LIBLDFLAGSWIN)

dnl Functions
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_CHECK_FUNCS([setlocale])
AC_CHECK_FUNCS([mbrlen])
AC_CHECK_FUNCS([memset])
AC_CHECK_FUNCS([strchr])
AC_CHECK_FUNCS([getpwuid_r])
AM_ICONV

dnl Default compiler settings
CFLAGS="$CFLAGS -Wall -Werror -pedantic -std=c99 -D_POSIX_C_SOURCE=200112L"
CXXFLAGS="$CXXFLAGS -Wall -Werror -pedantic"

AC_CONFIG_FILES([Makefile src/Makefile src/tools/Makefile src/libvoikko.pc])
AC_OUTPUT
