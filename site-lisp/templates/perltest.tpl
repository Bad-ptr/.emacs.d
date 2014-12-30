#!/usr/bin/perl -T

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: GPL either version 2 or any later version


package Test(>>>FILE_SANS<<<)Test;

use utf8;
use strict;
use warnings FATAL => 'all';
use diagnostics;

#use encoding 'utf8';
use Encode;

use open qw(:std :utf8);

binmode STDOUT, ":utf8";
binmode STDIN, ":utf8";
binmode STDERR, ":utf8";

use feature qw(unicode_strings say);

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
