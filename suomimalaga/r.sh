#!/bin/bash

gawk 'BEGIN{n=0}{
  if ($0 ~ /combi_rule/) {
    print $2
  }
  else if ($0 ~ /rules/) {
    n = 1
  }
  if (n == 1) {
    print $0
  }
  if ($0 ~ /;/) {
    n = 0;
  }
}' suomi.mor

