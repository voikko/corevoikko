#!/usr/bin/perl -w

# Suomi-malaga, suomen kielen muoto-opin kuvaus.
#
# Tekijänoikeus © 2006-2007, 2011 Hannu Väisänen (Etunimi.Sukunimi@uef.fi)
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
# Software Foundation Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.
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
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
#
# Linking this program statically or dynamically with other modules is
# making a combined work based on this program.  Thus, the terms and
# conditions of the GNU General Public License cover the whole
# combination.


# Muutetaan tiedostot *.txt Malaga-koodiksi.
#
# Käyttö: ./txt.pl */*.txt
#         ./txt.pl -d [-D] */*.txt
#
#  -d tulostaa kommentteja generoituun tiedostoon virheiden korjausta varten.
#  -D tulostaa lisää kommentteja generoituun tiedostoon virheiden korjausta varten.

use strict;
use utf8;
use encoding 'utf8';
use Getopt::Std;

require 5.004;
use POSIX qw(locale_h);
setlocale (LC_ALL, "fi_FI.utf8");


my %options=();
getopts ("dD", \%options);
print "# -d $options{d}\n" if defined $options{d};
print "# -D $options{D}\n" if defined $options{D};

# Taivutusmuodoista johdettavat taivutusmuodot tai johtimet.
#
my %table = (
  "nimitapa_3"       => "johdin_mA",
  "nimitapa_3_saama" => "johdin_mA_saama",
  "laatutapa_1_vA"   => "johdin_vA johdin_vAinen",
  "laatutapa_1_vE"   => "johdin_vE johdin_vAinen",
  "laatutapa_1_tA"   => "johdin_tAvA johdin_tAvAinen",
  "laatutapa_1_ttA"  => "johdin_ttAvA johdin_ttAvAinen",
  "laatutapa_2_lUt"  => "johdin_lUt",
  "laatutapa_2_nUt"  => "johdin_nUt",
  "laatutapa_2_rUt"  => "johdin_rUt",
  "laatutapa_2_sUt"  => "johdin_sUt",
  "laatutapa_2_tU_dU"          => "johdin_tU_dU",
  "laatutapa_2_tU_lU_katseltu" => "johdin_tU_lU_katseltu",
  "laatutapa_2_tU_lU_oltu"     => "johdin_tU_lU_oltu",
  "laatutapa_2_tU_nU"          => "johdin_tU_nU",
  "laatutapa_2_tU_rU"          => "johdin_tU_rU",
  "laatutapa_2_stU_juostu"     => "johdin_stU_juostu",
  "laatutapa_2_stU_nuolaistu"  => "johdin_stU_nuolaistu",
  "laatutapa_2_ttU"         => "johdin_ttU",
  "tositavan_kestämä_dAAn"  => "tositavan_kestämä_dAAn_kielto",
  "tositavan_kestämä_lAAn"  => "tositavan_kestämä_lAAn_kielto",
  "tositavan_kestämä_nAAn"  => "tositavan_kestämä_nAAn_kielto",
  "tositavan_kestämä_rAAn"  => "tositavan_kestämä_rAAn_kielto",
  "tositavan_kestämä_tAAn"  => "tositavan_kestämä_tAAn_kielto",
  "mahtotapa_le"     => "mahtotapa_le_kielto",
  "mahtotapa_ne"     => "mahtotapa_ne_kielto",
  "mahtotapa_re"     => "mahtotapa_re_kielto",
  "mahtotapa_se"     => "mahtotapa_se_kielto",
  "käskytapa_kielto" => "käskytapa"
);



sub xprint
{
  my $s = shift @_;

#print "\n# xprint {", $s, "}\n";

  if ($s !~ /^[<][>]$/) {
    foreach my $i (@_) {
      my @a = split / +/, $i;
      foreach my $j (@a) {
        print ",\n";
        print "                         ", $j, ": ", $s;
      }
    }
  }
}


my %s1 = ("kestämän_tekijäpääte_y3" => "_y3");
my %s2 = ("kestämän_tekijäpääte_y3" => qr/[+]$/);


foreach my $file (@ARGV) {
  utf8::upgrade($file);
  if (defined $options{d}) {
    print "#== ", $file, "\n";
  }

  open (INFILE, $file) or die "Tiedosto $file ei aukea.\n";

  my %par = ();
  my $u = 1;
  my $n = 0;
  my %extra = ();
  my @dollar_par = ();
  my %dollar_par = ();

  $/ = "";


  # Tallennetaan kaikki sanojen vartaloitten muutokset
  # assosiatiiviseen taulukkoon map.
  #
  while (<INFILE>) {
    chomp;
    if (length($_) > 0) {
      if (defined $options{d}) {
        print "# ", $n++, " ", $_, "\n";
      }

      s/#.*//gs;   # Poistetaan kommentit.

      if (length($_) > 0) {
#      if (($_ !~ /#.*/) && (length($_) > 0)) {
        my ($head, @tail) = split /\n/;
        foreach my $i (@tail) {
          $i =~ s/^ *//;  # Poistetaan tyhjeet rivin alusta.
          my ($word, @flags) = split (/ +/, $i);
          if ($word =~ /[$][0-9]+/) {
            push @dollar_par, ("\$" . $head);
            $dollar_par{$word} = ("\$" . $head);
          }
          elsif ($word !~ /(\w|[+])+[+]itten/) {
if (defined $options{D}) {print "\n#D1 word ", $word, "\n";}
            $word =~ /[+]([^+]*)[+]/;
if (defined $options{D}) {print "\n#D2 word ", $word, " [", $1, "]\n";}
            if (!exists($par{$1})) {
              $par{$1} = sprintf ("%02d", $u++);
            }
          }
        }
      }
      $n++;
#print "\n", $_, " n=", $n, "\n";
    }
  }

  if (defined $options{D}) {
    while ((my $key, my $value) = each %par) {
      print "#D3 key ", $key, " value ", $value, "\n";
    }
  }

  close INFILE;


  if (defined $options{d}) {
    my @str;
    while ((my $key, my $value) = each %par) {
      push @str, ("param" . $value . " = \"" . $key . "\"");
    }
    for my $p (sort @str) {print "# ", $p, "\n";}
    print "#==";
    for my $p (sort @str) {
      print " ";
      if ($p =~ /""/) {
        print "\"\"";
      }
      elsif ($p =~ /"([^"]*)"/) {
        print $1;
      }
    }
    print "\n";
  }


  # Tulostetaan säännön nimi ja parametrilista.
  #
  $file =~ m&([^./0-9]+)[.]txt$&;
  print "subrule ", $1, " (\$tietue";
if (defined $options{D}) {print "\n#A0 [", $1, "][", $file, "]\n";}
  for my $i (1 .. $u-1) {
    printf (", \$param%02d", $i);
  }
  for my $i (@dollar_par) {
    print ", ", $i;
  }

  print "):\n";
##  print "  define \$a := transmit (\$tietue.perusmuoto + \" $1\");\n";
  print "  return taivutuskaava (\$tietue,\n";


  open (INFILE, $file) or die "Tiedosto $file ei aukea.\n";


  my $m = 0;


  # Generoidaan taivutuksen Malaga-koodi.
  #
  while (<INFILE>) {
    chomp;
    if (length($_) > 0) {
      if (defined $options{D}) {
        print "\n#D4 ", $n++, " ", $_, "\n";
      }

      s/#.*//gs;   # Poistetaan kommentit.

      if (length($_) > 0) {
#      if (($_ !~ /#.*/) && (length($_) > 0)) {
        my $first = ($m == 0) ? "                        [" : ",\n                         ";

        my ($head, @tail) = split /\n/;

        my @x;
        foreach my $i (@tail) {
          $i =~ s/^ +//;
          my ($word, @flags) = split (/ +/, $i);

          if ($word =~ /[$][0-9]+/) {
            push @x, ("<" . $dollar_par{$word} . ", <" . join(",",@flags) . ">>");
          }
          elsif (($word !~ /[$][0-9]+/) && ($word !~ /(\w|[+])+[+]itten/)) {
            $word =~ /[+]([^+]*)[+]/;
            my $z = $1;
            if (exists($par{$z})) {
              if (exists($s1{$head}) && ($word =~ $s2{$head})) {
                push @flags, $s1{$head};
if (defined $options{D}) {print "\nhead [", $head, "] [", $word, "]\n";}
              }
              push @x, ("<\$param" . $par{$z} . ", <" . join(",",@flags) . ">>");
            }
            else {
              die "'\$z' ei ole olemassa. Jotain on pahasti vialla (", $word, ")\n";
            }
          }
if (defined $options{D}) {print "\n#D5 head [", $head, "] [", $word, "]\n";}
        }
        $m++;
if (defined $options{D}) {print "\n#D6 ", $_, " m=", $m, "\n";}

        my $s = "<" . join(", ", @x) . ">";


if (defined $options{D}) {print "\n#D7 s    [", $s, "]\n";}

        $extra{$head} = $s;

        if (($head ne "yhdyssana")) {
          if ($s !~ /^[<][>]$/) {
            print $first, $head, ": ", $s;
          }
          if ($head eq "kestämän_tekijäpääte_y3") {
            #
            # Päätteet pi/vi: punoa, hän punoo => hän punoopi/punoovi/punovi.
            # Sanoille, joiden pääte on sulautunut vartaloon, on
            # vain pääte pi. Esim. voida, hän voi => hän voipi.
            #
            if ($s =~ /_y3/) {
              my @is_y3;
              my @is_not_y3;
              for my $i (@x) {
                if ($i =~ /_y3/) {
                  push @is_y3, $i;
                }
                else {
                  push @is_not_y3, $i;
                }
              }
              my $s_y3     = "<" . join(", ", @is_y3)     . ">";
              my $s_not_y3 = "<" . join(", ", @is_not_y3) . ">";

              print $first, $head, "_vi: ",  $s_not_y3;  # Punovi.
              print $first, $head, "_Vpi: ", $s_not_y3;  # Punoopi.
              print $first, $head, "_Vvi: ", $s_not_y3;  # Punoovi.
              print $first, $head, "_pi: " , $s_y3;      # Voipi.
            }
            else {
              print $first, $head, "_vi: ",  $s;   # Punovi.
              print $first, $head, "_Vpi: ", $s;   # Punoopi.
              print $first, $head, "_Vvi: ", $s;   # Punoovi.
            }
          }
        }

if (defined $options{D}) {print "\n#D8 [[", $_, "]] ", $m, " ", $n, "\n"};

        if ($m + 1 == $n) {
          if (exists($extra{"yhdyssana"})) {
###            xprint ($extra{"yhdyssana"}, "yhdyssana nimisana nimi_laatusana laatusana teonsana etuliite tavuviiva");
###            xprint ($extra{"yhdyssana"}, "nimisana nimi_laatusana laatusana teonsana etuliite tavuviiva");
            xprint ($extra{"yhdyssana"}, "tavuviiva yhdyssana");
          }
          elsif (exists($extra{"nimentö"})) {
###            xprint ($extra{"nimentö"}, "yhdyssana nimisana nimi_laatusana laatusana teonsana etuliite tavuviiva");
###            xprint ($extra{"nimentö"}, "nimisana nimi_laatusana laatusana teonsana etuliite tavuviiva");
            xprint ($extra{"nimentö"}, "tavuviiva yhdyssana");
          }

          while ((my $key, my $value) = each %table) {
            if (exists($extra{$key})) {
              xprint ($extra{$key}, $value);
            }
          }
          print "]);\n";
        }
      }
     }
  }
  print "end;\n";

  close INFILE;

  %par = ();
  $u = 1;
  %extra = ();
  @dollar_par = ();
  %dollar_par = ();
}
