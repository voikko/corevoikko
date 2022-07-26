#!/bin/sh
sudo apt-get install hfst-ospell-dev libtool automake pkg-config gettext
cd libvoikko && ./autogen.sh && ./configure && make
cd java && mvn compile
cd ../cs && xbuild libvoikko.sln
