# Sanan viimeinen kirjain.
# Tekijä: Björn Beutel.
#
subrule last ($string):
  ! $string matches ".*", ".": $last;
  return $last;
end;
