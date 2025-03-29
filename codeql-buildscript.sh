#!/bin/sh
sudo apt-get install hfst-ospell-dev libtool automake pkg-config gettext mono-xbuild libnunit-cil-dev
cd libvoikko && ./autogen.sh && ./configure && make
cd java && mvn compile
# C# is disabled for now, nunit no longer in debian
# cd ../cs && xbuild libvoikko.sln
