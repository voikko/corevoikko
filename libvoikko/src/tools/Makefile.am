bin_PROGRAMS = voikkospell voikkohyphenate voikkogc

TOOLCXXFLAGS = $(AM_CXXFLAGS)
TOOLLDADD = ../libvoikko.la

voikkospell_SOURCES = voikkospell.cpp
voikkospell_CXXFLAGS = $(TOOLCXXFLAGS)
voikkospell_LDADD = $(TOOLLDADD)

if HAVE_PTHREAD
    voikkospell_CXXFLAGS += $(PTHREAD_CFLAGS) -DHAVE_PTHREAD
    voikkospell_LDADD += $(PTHREAD_LIBS)
endif
if HAVE_HFST 
    voikkospell_CXXFLAGS += -lhfst-ospell
    voikkospell_LDADD += -lhfst-ospell
endif
voikkohyphenate_SOURCES = voikkohyphenate.cpp
voikkohyphenate_CXXFLAGS = $(TOOLCXXFLAGS)
voikkohyphenate_LDADD = $(TOOLLDADD)

voikkogc_SOURCES = voikkogc.cpp
voikkogc_CXXFLAGS = $(TOOLCXXFLAGS)
voikkogc_LDADD = $(TOOLLDADD)

dist_man_MANS = voikkospell.1 voikkohyphenate.1 voikkogc.1

