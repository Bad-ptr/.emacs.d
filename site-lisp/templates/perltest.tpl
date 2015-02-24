#!/usr/bin/perl -T

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: (>>>LICENSE<<<)


package Test(>>>FILE_SANS<<<)Test 1.0;

use (>>>PERL_VERSION<<<);

(>>>PERL_DEFAULT_USES<<<)

use utf8;
use strict;
use warnings FATAL => 'all';
#no warnings 'experimental';
use diagnostics;

#use encoding 'utf8';
use Encode;

use open qw(:std :utf8);

binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
binmode STDERR, ":utf8";

use feature qw(unicode_strings say switch);

# use Scalar::Util qw(blessed reftype looks_like_number); #set_prototype
# use List::Util
# use Try::Tiny;

use Data::Dumper;
use Carp;

use Test::More;


__PACKAGE__->run(@ARGV) unless caller();

sub run {
  my $self = shift;

  use lib '../../lib';

  BEGIN {
      use_ok '(>>>FILE_SANS<<<)' || print "Bail out!\n";
  }
  diag ("Testing (>>>FILE_SANS<<<) $(>>>FILE_SANS<<<)::VERSION, Perl $], $^X");

  (>>>POINT<<<)

  done_testing();
}

