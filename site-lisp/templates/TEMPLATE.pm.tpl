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
# no warnings 'experimental';

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
# use Data::Dumper;

use Carp;

use parent 'Exporter';
our @EXPORT=qw();
our @EXPORT_OK=qw();


#__PACKAGE__->run(@ARGV) unless caller();


sub new ($%) {
  my $self = shift;
  my $class = ref $self || $self;

  my $ret = bless {}, $class;
  return $ret->_init(@_);
}

sub _init ($%) {
  my $self = shift;
  my ($params) = @_;
  unless (ref $params eq 'HASH') {
    my %th = @_;
    $params = \%th;
  }

  $self->{$_} = $params->{$_} foreach keys $params;

  return $self;
}

(>>>POINT<<<)


1;
