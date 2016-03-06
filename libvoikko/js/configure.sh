#!/bin/sh

CXXFLAGS="-g0 -O2" emconfigure ./configure --with-dictionary-path=/ --disable-hfst --disable-buildtools --disable-testtools --disable-assert
