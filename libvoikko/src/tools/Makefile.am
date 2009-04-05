bin_PROGRAMS = voikkospell voikkohyphenate voikkogc

voikkospell_SOURCES = voikkospell.cpp
voikkospell_CXXFLAGS = $(AM_CXXFLAGS) -Wl,--as-needed
voikkospell_LDADD = ../libvoikko.la

voikkohyphenate_SOURCES = voikkohyphenate.cpp
voikkohyphenate_CXXFLAGS = $(AM_CXXFLAGS) -Wl,--as-needed
voikkohyphenate_LDADD = ../libvoikko.la

voikkogc_SOURCES = voikkogc.cpp
voikkogc_CXXFLAGS = $(AM_CXXFLAGS) -Wl,--as-needed
voikkogc_LDADD = ../libvoikko.la

dist_man_MANS = voikkospell.1 voikkohyphenate.1 voikkogc.1

