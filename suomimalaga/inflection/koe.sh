#!/bin/sh

# Testataan sanojen tunnistusta.

grep -v '#' $* \
| fmt -1 \
| grep -F '+' \
| grep -v '^[+]' \
| sed -e "s/[+]//g" \
| grep -v 'inen$' \
| malaga -m ../sukija/suomi.pro
