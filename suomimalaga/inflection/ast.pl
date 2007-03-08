#!/usr/bin/perl -w

# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006 Hannu Väisänen (Etunimi.Sukunimi@joensuu.fi)
#
# Tämä ohjelma on vapaa; tätä ohjelmaa on sallittu levittää
# edelleen ja muuttaa GNU yleisen lisenssin (GPL lisenssin)
# ehtojen mukaan sellaisina kuin Free Software Foundation
# on ne julkaissut; joko Lisenssin version 2, tai (valinnan
# mukaan) minkä tahansa myöhemmän version mukaisesti.
#
# Tätä ohjelmaa levitetään siinä toivossa, että se olisi
# hyödyllinen, mutta ilman mitään takuuta; ilman edes
# hiljaista takuuta kaupallisesti hyväksyttävästä laadusta tai
# soveltuvuudesta tiettyyn tarkoitukseen. Katso GPL
# lisenssistä lisää yksityiskohtia.
#
# Tämän ohjelman mukana pitäisi tulla kopio GPL
# lisenssistä; jos näin ei ole, kirjoita osoitteeseen Free
# Software Foundation Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
#
# Tämän ohjeman linkittäminen staattisesti tai dynaamisesti
# muihin moduuleihin on ohjelmaan perustuvan teoksen
# tekemistä, joka on siis GPL lisenssin ehtojen alainen.
#
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place -  Suite 330, Boston, MA
# 02111-1307 USA.
#
# Linking this program statically or dynamically with other modules is
# making a combined work based on this program.  Thus, the terms and
# conditions of the GNU General Public License cover the whole
# combination.


# Muutetaan tiedostot *.ast Malaga-koodiksi.
#
# Käyttö: ./ast.pl a/*.ast b/*.ast
#          /ast.pl -d a/*.ast b/*.ast
#
#  -d tulostaa kommentteja generoituun tiedostoon virheiden korjausta varten.

use strict;
use utf8;
use encoding 'utf8';
use Getopt::Std;

require 5.004;
use POSIX qw(locale_h);
setlocale (LC_ALL, "fi_FI.utf8");

my %options=();
getopts ("d", \%options);
print "# -d $options{d}\n" if defined $options{d};

my %map;


my %q = ("a" => "A", "o" => "O", "u" => "U",
         "ä" => "A", "ö" => "O", "y" => "U");

my %r = ("A" => "A(\$tietue)+", "O" => "O(\$tietue)+", "U" => "U(\$tietue)+");

sub convert($)
{
  my $s = $_[0];
  $s =~ s/([aouäöy])/$q{$1}/eg;   # 'ab         => 'Ab
  $s =~ s/(([^AOU]|')+)/"$1"+/g;  # 'Ab         => "'"+A+"b"+
  $s =~ s/([AOU])/$r{$1}/eg;      # "'"+A+"b"+  => "'"+A($tietue)+"b"+
  $s =~ s/""""/""/g;              # Muutetaan 4 peräkkäistä lainausmerkkiä 2:ksi.
  $s =~ s/\+$//;                  # Poistetaan plus-merkki s:n lopusta.
  return $s;
}


# Tallennetaan kaikki sanojen vartaloitten muutokset
# assosiatiiviseen taulukkoon map.
#
foreach my $file (@ARGV) {
  utf8::upgrade($file);

  if (defined $options{d}) {
    print "#== ", $file, "\n";
  }

  open (INFILE, $file) or die "Tiedosto $file ei aukea.\n";

  while (<INFILE>) {
    chomp;

    s/#.*$//g;   # Poistetaan kommentit.

    if (length($_) > 0) {
      my ($word, $subrule, @param) = split;

#print "param @param\n";

      my $flag = 0;

      foreach my $i (@param) {
        if ($i !~ /^%/) {
          $map{$i} = 1;
        }
      }
    }
  }

  close INFILE;
}


my %arg;


# Muutetaan taulukko 'map' Malagan parametrilistaksi muuttujaan 'arg'.
#
foreach my $key (sort (keys %map)) {
  my $s = $key;

  $s =~ s/([aouäöy])/$q{$1}/eg;   # 'ab         => 'Ab
  $s =~ s/(([^AOU]|')+)/"$1"+/g;  # 'Ab         => "'"+A+"b"+
  $s =~ s/([AOU])/$r{$1}/eg;      # "'"+A+"b"+  => "'"+A($tietue)+"b"+
  $s =~ s/""""/""/g;              # Muutetaan 4 peräkkäistä lainausmerkkiä 2:ksi.
  $s =~ s/\+$//;                  # Poistetaan plus-merkki s:n lopusta.
  $arg{$key} = $s;
#  print $key, " ", $s, "\n";
}


if (defined $options{d}) {
  foreach my $key (sort (keys %arg)) {
    print "# ", $key, " ", $arg{$key}, "\n";
  }
}


# Generoidaan Malaga-koodi.
#
foreach my $file (@ARGV) {
  utf8::upgrade($file);
  if (defined $options{d}) {
    print "#== ", $file, "\n";
  }

  open (INFILE, $file) or die "Tiedosto $file ei aukea.\n";

  while (<INFILE>) {
    chomp;

    s/#.*$//g;   # Poistetaan kommentit.

    if (length($_) > 0) {
      if (defined $options{d}) {
        print "# ", $_, "\n";
      }

      my ($word, $subrule, @param) = split;

      if (defined $options{d}) {
        foreach my $i (@param) {
          if ($i !~ /^%/) {
            print "# ", $i, " ", $arg{$i}, "\n";
          }
        }
      }

      print "  elseif (", $word, " in \$tietue.jatko) then\n";
      print "    choose \$i in ", $subrule, " (\$tietue";

      foreach my $i (@param) {
        if ($i !~ /^%/) {
          print ", ", $arg{$i};
        }
      }
      print ")";
      foreach my $i (@param) {
        if ($i =~ /^%/) {
          my $rule = substr($i,1);
          if ($rule =~ /[)]$/) {
            $rule =~ /[(]([^)]+)[)]/;
            my @p = split /,/, $1;
            print "\n                 + ", substr($rule,0,index($rule,"(")), " (\$tietue";
            foreach my $j (@p) {
             if ($j !~ /^""$/) {  # Jos argumentti ei ole "", poistetaan lainausmerkit.
               $j =~ s/"//g;
             }
             print ", ", convert($j);
            }
            print ")";
          }
          else {
            print "\n                 + ", $rule, " (\$tietue)";
          }
        }
      }
      print ";\n";
      print "    if (value_type(\$i) = record) then\n";
      print "      result \$i.alku, \$i - alku;\n";
      print "    else\n";
      print "      foreach \$j in \$i:\n";
#      print "define \$a := transmit (<\"a\"> + <value_type(\$i)>);\n";
#      print "define \$b := transmit (<\"b\"> + <value_type(\$j)>);\n";
      print "        result \$j.alku, \$j - alku;\n";
      print "      end;\n";
      print "    end;\n";
    }
  }
  close INFILE;
}
