#!/bin/sh

echo Cleaning autotools files...
find . -type d -name autom4te.cache | xargs rm -rf \;
find . -type f \( -name missing -o -name install-sh -o -name mkinstalldirs \
        -o -name depcomp -o -name ltmain.sh -o -name configure \
        -o -name config.sub -o -name config.guess \
        -o -name Makefile.in \) | xargs rm -f

rm -f config.sub config.guess aclocal.m4 config.h.in config.rpath
rm -f m4/libtool.m4 m4/lt~obsolete.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4
cp /usr/share/gettext/config.rpath .

echo Creating autotools files...
autoreconf --force --install

