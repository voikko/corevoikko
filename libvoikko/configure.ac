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
AC_INIT([libvoikko],[4.0.2],[hatapitk@iki.fi])

LT_PREREQ([2.2.6])

AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE([1.10 foreign])
AC_CONFIG_MACRO_DIR([m4])

dnl Checks for programs
AC_PROG_CXX
AC_PROG_INSTALL
AM_PATH_PYTHON([3])

dnl Libtool (new and old)
AC_LIBTOOL_WIN32_DLL
LT_INIT

AM_MAINTAINER_MODE

dnl Compiler features
AC_LANG_CPLUSPLUS
AC_C_CONST
AX_CXX_COMPILE_STDCXX(11, noext, mandatory)

# Prepare platform specific setups (sets $host_os a.o.):
AC_CANONICAL_HOST

# The default dictionary path is empty (no default locations to search from)
DICTIONARY_PATH=
AC_ARG_WITH(dictionary-path,
            AC_HELP_STRING([--with-dictionary-path=PATH],
	                   [Dictionary path]),
	    [DICTIONARY_PATH="$withval"])
AC_DEFINE_UNQUOTED(DICTIONARY_PATH, "$DICTIONARY_PATH", [Dictionary path])
AC_SUBST(DICTIONARY_PATH)

dnl Checks for headers
AC_HEADER_ASSERT
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
buildtools="yes"
AC_ARG_ENABLE(buildtools, AC_HELP_STRING([--disable-buildtools],
        [disable voikkovfstc]),
        [buildtools=${enableval}], [buildtools=yes])
AM_CONDITIONAL([HAVE_BUILDTOOLS], [test x$buildtools = xyes])

dnl Test tools
testtools="yes"
AC_ARG_ENABLE(testtools, AC_HELP_STRING([--disable-testtools],
	[disable voikkospell, voikkohyphenate and voikkogc]),
	[testtools=${enableval}], [testtools=yes])
AM_CONDITIONAL([HAVE_TESTTOOLS], [test x$testtools = xyes])
	
dnl Malaga support
malaga="no"
AH_TEMPLATE(HAVE_MALAGA, [Have MALAGA])
AC_ARG_ENABLE(malaga, AC_HELP_STRING([--enable-malaga],
	[enable Malaga morphology backend (dictionary version 2)]),
	[malaga=${enableval}], [malaga=no])
if test x$malaga = xyes; then
	AC_DEFINE(HAVE_MALAGA, 1)
fi
AM_CONDITIONAL([HAVE_MALAGA], [test x$malaga = xyes])

dnl HFST support
hfst="yes"
AH_TEMPLATE(HAVE_HFST, [Have HFST])
AC_ARG_ENABLE(hfst, AC_HELP_STRING([--disable-hfst],
	[disable HFST morphology backend]),
	[hfst=${enableval}], [hfst=yes])
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
	PKG_CHECK_MODULES([VISLCG3], [cg3 >= 0.9])
	AC_CHECK_HEADERS([tinyxml2.h],,[AC_MSG_ERROR([tinyxml2.h not found])])
	AC_DEFINE(HAVE_VISLCG3, 1)
	CXXFLAGS="$CXXFLAGS $VISLCG3_CFLAGS"
	])
AM_CONDITIONAL([HAVE_VISLCG3], [test x$cg3 = xyes])


dnl VFST support
vfst="yes"
AH_TEMPLATE(HAVE_VFST, [Have VFST])
AC_ARG_ENABLE(vfst, AC_HELP_STRING([--disable-vfst],
	[disable VFST morphology backend]),
	[vfst=${enableval}], [vfst=yes])
if test x$vfst = xyes; then
	AC_DEFINE(HAVE_VFST, 1)
fi
AM_CONDITIONAL([HAVE_VFST], [test x$vfst = xyes])


dnl support for experimental parts of VFST format
expvfst="no"
AH_TEMPLATE(HAVE_EXPERIMENTAL_VFST, [Have experimental VFST])
AC_ARG_ENABLE(expvfst, AC_HELP_STRING([--enable-expvfst],
	[enable EXPERIMENTAL features of VFST morphology backend]),
	[expvfst=${enableval}], [expvfst=no])
if test x$expvfst = xyes; then
	AC_DEFINE(HAVE_EXPERIMENTAL_VFST, 1)
fi
AM_CONDITIONAL([HAVE_EXPERIMENTAL_VFST], [test x$expvfst = xyes])

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

AX_PTHREAD([AM_CONDITIONAL([HAVE_PTHREAD],[true])], [AM_CONDITIONAL([HAVE_PTHREAD],[false])])

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

cat <<EOF

Libvoikko was configured with the following options
  * VFST support:                   $vfst
  *   Experimental VFST features:   $expvfst
  * HFST support:                   $hfst
  * Malaga support:                 $malaga
  * Experimental VISLCG3 support:   $cg3
  * Experimental Lttoolbox support: $lttoolbox
  * Morphology compilers:           $buildtools
  * Simple client programs:         $testtools
EOF
if test x$DICTIONARY_PATH = x; then
cat <<EOF
  * Fallback dictionary path:       (none)
  NOTE!  As of libvoikko 4.0 NO HARDCODED FALLBACK DICTIONARY PATH
  NOTE!  IS USED unless one is specified using --with-dictionary-path.
  NOTE!  If you intend to use libvoikko for writing aids in
  NOTE!  end user applications we strongly suggest adding the following:
  NOTE!  OS X: --with-dictionary-path=/Library/Spelling/voikko:/usr/lib/voikko
  NOTE!  Linux: --with-dictionary-path=/usr/share/voikko:/usr/lib/voikko
EOF
else
cat <<EOF
  * Fallback dictionary path:       $DICTIONARY_PATH
EOF
fi
