#!/bin/sh

# Automake version.
AM_VERSION_SUFFIX=-1.10

echo Cleaning autotools files...
find -type d -name autom4te.cache -print0 | xargs -0 rm -rf \;
find -type f \( -name missing -o -name install-sh -o -name mkinstalldirs \
        -o -name depcomp -o -name ltmain.sh -o -name configure \
        -o -name config.sub -o -name config.guess \
        -o -name Makefile.in \) -print0 | xargs -0 rm -f

rm -f config.sub config.guess aclocal.m4 config.h.in config.rpath
rm -rf m4
cp /usr/share/misc/config.sub .
cp /usr/share/misc/config.guess .
cp /usr/share/gettext/config.rpath .

libtoolize --force --copy
autoheader
aclocal$AM_VERSION_SUFFIX 
automake$AM_VERSION_SUFFIX --add-missing --copy --force-missing --foreign
autoconf
autoheader
#autoreconf

