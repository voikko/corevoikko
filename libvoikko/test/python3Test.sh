#!/bin/bash
# Tests that the Python interface is compatible with Python 3.

TMPTESTDIR=python3test

mkdir $TMPTESTDIR

cp python/libvoikko.py test/libvoikkoTest.py test/TestUtils.py $TMPTESTDIR/
2to3 -w -n --no-diffs $TMPTESTDIR

export PYTHONPATH=${TMPTESTDIR}${PYTHONPATH:+:$PYTHONPATH}
python3 $TMPTESTDIR/libvoikkoTest.py

rm -f $TMPTESTDIR/libvoikko.py $TMPTESTDIR/libvoikko.pyc
rm -f $TMPTESTDIR/libvoikkoTest.py $TMPTESTDIR/libvoikkoTest.pyc
rm -f $TMPTESTDIR/TestUtils.py $TMPTESTDIR/TestUtils.pyc
rmdir $TMPTESTDIR
