#!/bin/bash

# Tulosta sanat, joiden perusmuoto ja jatko ovat samat.

gawk 'substr($2,2,length($2)-3) == substr($8,2,length($8)-3)' $1
