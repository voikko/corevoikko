#!/bin/sh

echo Cleaning autotools files...
find . -type d -name autom4te.cache | xargs rm -rf \;
find . -type f \( -name missing -o -name install-sh -o -name mkinstalldirs \
        -o -name depcomp -o -name ltmain.sh -o -name configure \
        -o -name config.sub -o -name config.guess \
        -o -name Makefile.in \) | xargs rm -f

rm -f config.sub config.guess aclocal.m4 config.h.in config.rpath
rm -rf m4
cp /usr/share/misc/config.sub .
cp /usr/share/misc/config.guess .
cp /usr/share/gettext/config.rpath .

echo Creating autotools files...
mkdir -p m4
autoreconf --force --install

