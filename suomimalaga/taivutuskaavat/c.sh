#!/bin/bash

for i in `ls nimisanat/*txt teonsanat/*txt`
do
  cat ../copyright.notice $i >$i.tmp
  mv $i.tmp $i
done
