dist_man_MANS =
bin_PROGRAMS =
EXTRA_DIST =
if HAVE_BUILDTOOLS
    bin_PROGRAMS += voikkovfstc
    dist_man_MANS += voikkovfstc.1
else
    EXTRA_DIST += voikkovfstc.1
endif
if HAVE_TESTTOOLS
    bin_PROGRAMS += voikkospell voikkohyphenate voikkogc
    dist_man_MANS += voikkospell.1 voikkohyphenate.1 voikkogc.1
else
    EXTRA_DIST += voikkospell.1 voikkohyphenate.1 voikkogc.1
endif

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
    voikkospell_CXXFLAGS += $(HFSTOSPELL_CFLAGS)
    voikkospell_LDADD += $(HFSTOSPELL_LIBS)
endif
voikkohyphenate_SOURCES = voikkohyphenate.cpp
voikkohyphenate_CXXFLAGS = $(TOOLCXXFLAGS)
voikkohyphenate_LDADD = $(TOOLLDADD)

voikkogc_SOURCES = voikkogc.cpp
voikkogc_CXXFLAGS = $(TOOLCXXFLAGS)
voikkogc_LDADD = $(TOOLLDADD)

voikkovfstc_SOURCES = voikkovfstc.cpp
voikkovfstc_CXXFLAGS = $(TOOLCXXFLAGS)
