# -*- coding: utf-8 -*-

# This file contains the preferences for Voikko
# developer tools. Copy this file somewhere within
# your Python module search path and modify as needed.
# You do not need to do this if the defaults suit you.

# Remember to uncomment the lines you modify.
# Note that the defaults may not be the same as the
# ones below. For example, the home directory of the
# user is determined programmatically by default.

# The location of Voikko SVN repository checkout
#svnroot='/home/uname/svn/voikko'

# Data directory for voikkotest. This directory must
# already exist. If you want to test spell checker between
# different revisions of the softwre this directory must
# contain test data as gzipped list of words separated by
# line feeds as 'wordlist.txt.gz'. Otherwise the directory
# may be empty.
#voikkotest_dir='/home/uname/tmp/voikkotest'

# Number of threads to use when running voikkospell on
# wordlist.txt.gz. The default is to use one thread.
#voikkospell_threads=1

# Language variant to use in the tests.
#language_variant='standard'

# Directory where Suomi-malaga is installed using
# "make voikko-install" after it has been built during automated
# tests. None means that installation is not performed.
#voikkotest_sm_destdir=None

# Directory containing tests and other necessary data files.
#voikko_data_dir='/home/uname/svn/voikko/trunk/data'

# The input/output encoding for programs that read
# or write data.
#encoding='UTF-8'

# The installation directory of libvoikko binaries. Note that
# this does not affect tests that are run through Python API.
# To have them run using a library in a non-standard installation
# directory you must set LD_LIBRARY_PATH appropriately.
#libvoikko_bin='/usr/bin'

# Command for viewing differences between 'base' and 'current'.
#diffviewcmd='diff -U 0 "%s" "%s" | grep ^.C: 2>/dev/null | less'
#diffviewcmd='vimdiff "%s" "%s"'
