#!/usr/local/bin/perl56  -I/home/systems/syb/work/publisher
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

###!/usr/local/share/perl -I/home/systems/syb/work/publisher

###################################################################################
# Script modified from davtest.pl, probably originally by Daniel Varela Santoalla.
# Modifications by Iain Russell,
# December 2002
#
# Takes one command-line parameter: the name/path of a file to be uploaded.
# Strange behaviour: it actually requires a dummy command-line parameter
#                    to be included after the real one.
###################################################################################

use strict;
use HTTP::DAV;


# defined these if not intending to supply them through the GUI.
# If the GUI does supply them (ie if they are specified on the command
# line), then the hardcoded values will be overridden.

my $username = "";
my $password = "";
my $bUsernameSupplied = 0;
my $site = "w3ec2";



################################
# Check command-line arguments #
################################

# print "Args: $#ARGV\n";

my $numArgs = $#ARGV + 1;


if ($numArgs < 1)
{
    print "\nDAV updater.\n";
    print "Usage: dav.pl [path]<file> [username] [password] [site]\n";
    exit -1;
}


# has a username & password been supplied?

if ($numArgs >= 3)
{
    $username = @ARGV[1];
    $password = @ARGV[2];
    $bUsernameSupplied = 1;
}


# how about a site?

if ($numArgs == 4)
{
    $site = @ARGV[3];
}


# filename is the first on the command line

my ($file) = @ARGV[0];



##################################
# Set up the paths to be updated #
##################################

my $url;
my $to;

if ($site eq "w3ec2")
{
    $url = "http://w3ec2.ecmwf.int:81/";
    $to  = $url . "/magics/magplus/" . $file;
}

elsif ($site eq "wedit")
{
    $url = "http://wedit.ecmwf.int:81/";
    $to  = $url . "publications/manuals/magics/magplus/" .$file;
}


my $d = HTTP::DAV->new();
my $from = $ENV{'PWD'} . "/" . $file;  

print ("From: $from\n");
print ("To:   $to\n");

###################
# Upload the file #
###################

$d->DebugLevel(1);

$d->credentials( -user=>$username,-pass =>$password,-url =>$url);
$d->open( -url=>"$url" );

$d->lock( -url=>"$to", -timeout=>"10m" )
    or die "Won't put unless I can lock\n";

if ( $d->put( -local=>"$from", -url=>"$to") ) {
    print "$from successfully uploaded to $to\n";
} else {
    print "put failed: " . $d->message . "\n";
}

$d->unlock( -url=>"$to");
