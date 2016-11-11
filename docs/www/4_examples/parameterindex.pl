#!/usr/bin/env perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


use common::mpp_utils;

use XML::Parser;
use Data::Dumper;


my $ecmwf=file_to_string("common/ecmwf.htmlx");
my $info = {};
my $element = {};
my $dest="html/parameters/index.html";
my $page_title = "Magics++ Parameters";


sub parse
{
    my $def = shift;
    my $node = shift;   
   
    while ( defined ( $element = shift @{ $node } ) )
    {
       
        my $child = shift @{ $node };
        if ( ref $child )
        {
           my $attr = \%{ shift @{ $child } };
           my $name = $attr->{name};
           
           if ($name ne "") 
           {
               
               $def->{$element}->{$name} = {};
               my $list = $element . "_list";
               push( @{$def->{$list}}, $name);
               foreach my $a (keys %{$attr}) 
               {
                   $def->{$element}->{$name}->{attributes}->{$a} = $attr->{$a}; 
               }
               parse($def->{$element}->{$name}, $child); 
               
           }
           else
           {
                   $def->{$element} = {};
                   foreach my $a (keys %{$attr}) 
                   {
                        $def->{$element}->{attributes}->{$a} = $attr->{$a};
                   }
                   parse($def->{$element}, $child); 
           }
        }
        else 
        {
         
          $def->{data} = $child;
        }
    }   
}



sub text {
	my ($begin,$name,$default,$end) = @_;
	return $begin . &$name($default) . $end;
}

sub meta {
	my ($default) = @_;
	return $default;
}

sub metalink {
	my ($default) = @_;
	return $default;
}

sub doctitle {
	my ($default) = @_;
	$default = "<TITLE>" . $page_title . "</TITLE>";
	return $default;
}

sub customstyle {
	my ($default) = @_;
	return $default;
}

sub spare1 {
	my ($default) = @_;
	return $default;
}

sub spare2 {
	my ($default) = @_;
	return $default;
}

sub location {
	my ($default) = @_;
	$default = $default . file_to_string("common/location.htmlx");
	$default = $default . "Parameters";
	return $default;
}

sub topprevnext {
	my ($default) = @_;
	return $default;
}

sub botprevnext {
	my ($default) = @_;
	return $default;
}

sub heading {
	my ($default) = @_;
    $default = $page_title;
	return $default;
}
 
sub submenu {
	my ($default) = @_;
   
    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<tbody>" 
               . "<tr>"
               . "  <td><span class=\"menuheading\"><font color=\"#000000\">Browse</font></span></td>"
               . "</tr>"
               . "<tr>" 
               . "  <td> "
               . "    <table bgcolor=\"#ffffff\" border=\"0\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">"
               . "      <tr>" 
               . "        <td>" 
               . "            <table bgcolor=\"#ffffff\" width=\"100%\">";
        

     $default = $default . "</table>"
                . "        </table>"
                . "    </td>"
                . "   </tr>"
                . "   </table>";
                   
      
	return $default;
}

sub content {
	my ($default) = @_;
  
    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<tbody>" 
               . "  <tr>" 
                . "    <td  class=\"menuheading\">Magics++ Parameters </a> </td>"
               . "  </tr>" 
               . "  <tr>" 
                . "    <td   bgcolor=\"#ffffff\">
    <P><P>
	The Magics++ FORTRAN and MagML parameters are presented differently.
    <P>
    <a href=\"fortranactionindex.html\">FORTRAN Parameters</a>
    <P>
    <a href=\"../interfaces/magml/outline.html\">MagML Parameters</a>
    </td>"
               . "  <tr>" 
			   . " <td   bgcolor=\"#ffffff\"> &nbsp; </td>"
               . "  </tr>";
               

              
    $default = $default .   "</tbody> "
                . "</table>";
                
                
	return $default;
}

sub related {
	my ($default) = @_;
	return $default;
}

sub version {
	my ($default) = @_;
	$default = current_date();
	return $default;
}

sub info {
    return file_to_string("common/info.htmlx");
}

sub editor {
    return file_to_string("common/author.htmlx");
}




$/ = undef;
open(IN,file_to_string("common/template.htmlx"));
my $text = <IN>;
close(IN);
$text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

open HTML, ">" . $dest;
print HTML $text;
close HTML;




