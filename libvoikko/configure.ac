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
AC_PREREQ(2.59)
AC_INIT([libvoikko],[0.1],[hatapitk@iki.fi])
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE([1.9 foreign])

dnl Checks for programs
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL
#AC_PROG_RANLIB

MALAGA_PATH=${prefix}
AC_ARG_WITH(malaga-path,
            AC_HELP_STRING([--with-malaga-path=PATH],
	                   [Malaga installation prefix]),
            [MALAGA_PATH="$withval"])
AC_DEFINE_UNQUOTED(MALAGA_PATH, "$MALAGA_PATH", [Malaga installation prefix])
AC_SUBST(MALAGA_PATH)
				       
DICTIONARY_PATH=${libdir}/voikko
AC_ARG_WITH(dictionary-path,
            AC_HELP_STRING([--with-dictionary-path=PATH],
	                   [Dictionary path]),
	    [DICTIONARY_PATH="$withval"])
AC_DEFINE_UNQUOTED(DICTIONARY_PATH, "$DICTIONARY_PATH", [Dictionary path])
AC_SUBST(MALAGA_PATH)
						   

dnl Checks for libraries
#AC_CHECK_LIB(malaga, init_libmalaga, ,[AC_MSG_ERROR([libmalaga not found])])

dnl Checks for headers
AC_CHECK_HEADERS([stddef.h])
AC_CHECK_HEADERS([langinfo.h])
AC_CHECK_HEADERS([locale.h])
AC_CHECK_HEADERS([wctype.h])
AC_CHECK_HEADERS([wchar.h])
AC_CHECK_HEADER($MALAGA_PATH/include/malaga.h, , [AC_MSG_ERROR([malaga.h not found])])

dnl Types
AC_TYPE_SIZE_T

dnl Compiler features
AC_C_CONST

dnl Functions
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([nl_langinfo])
AC_CHECK_FUNCS([setlocale])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT

