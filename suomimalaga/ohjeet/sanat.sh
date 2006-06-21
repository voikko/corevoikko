#!/bin/bash

fmt -1 $* \
| sed -e "s/[^[:alpha:]]//g" \
| tr '[:upper:]' '[:lower:]' \
| sort \
| uniq \
| malaga -m suomi.pro
