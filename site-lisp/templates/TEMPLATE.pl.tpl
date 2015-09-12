#!/usr/bin/perl -w

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: (>>>LICENSE<<<)


package (>>>PERL_PACKAGE_NAME<<<) 1.0;

use (>>>PERL_VERSION<<<);

use utf8;
use strict;
use warnings;
no warnings 'experimental';

#use encoding 'utf8';
use Encode;

#use open qw(:std :utf8);

# binmode STDOUT, ":utf8";
# binmode STDIN, ":utf8";
# binmode STDERR, ":utf8";
use open qw( :encoding(UTF-8) :std );
binmode(DATA, ":encoding(UTF-8)");

use charnames qw( :full :short );


use feature qw(unicode_strings say switch);

# use Scalar::Util qw(blessed reftype looks_like_number); #set_prototype
# use List::Util
# use Try::Tiny;
# use Data::Dumper;
# use Carp;


__PACKAGE__->run(@ARGV) unless caller();


sub run {
  (>>>POINT<<<)
}


