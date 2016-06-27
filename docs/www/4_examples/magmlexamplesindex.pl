#!/usr/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


use common::mpp_utils;

my $ecmwf=file_to_string("common/ecmwf.htmlx");
my @menu = magml_testsuite_menu ();
my $dest="html/examples/magml/index.html";
my $page_title = "Magics++ MagML Programming Examples";




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
	$default = $default . file_to_string("common/location-test.htmlx");
	$default = $default . "MagML";
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
        

	foreach my $item (@menu)
    {
        my $title   = $item->{name};	
        my $html    = $item->{html};
        my $source  = $item->{source};
        my $fordocs = ($audience eq "internal")
                      ? $item->{fordocs}
                      : $item->{ext};
        
        if ($fordocs eq "yes")
        {        
            if ($html eq "")
            {
                $html = $source . ".html"
            }

            $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$html\">$title</a></td> </tr>\n";
        }
	}
    
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
                . "    <td   bgcolor=\"#ffffff\">
    <P><P>
	The following example MagML descriptions illustrate some of the features
	of MagML and how to use it. The code here can be copied and adapted to form
	the basis of your own MagML descriptions.
						</td>"
               . "  <tr>" 
			   . " <td   bgcolor=\"#ffffff\"> &nbsp; </td>"
               . "  </tr>" 
               . "  <tr>" 
                . "    <td  class=\"menuheading\"> Example Programs </a> </td>"
               . "  </tr>" 
               . "  </tr>";


    # -----------------------------------------------
    # Example programs menu at the bottom of the page
    # -----------------------------------------------

	foreach my $item (@menu)
    {
	    my $title   = $item->{name};	
	    my $html    = $item->{html};	
	    my $desc    = $item->{description};	
        my $source  = $item->{source};
        my $fordocs = ($audience eq "internal")
                      ? $item->{fordocs}
                      : $item->{ext};
        
        if ($fordocs eq "yes")
        {
            if ($html eq "")
            {
                $html = $source . ".html"
            }

            $default = $default . "         <tr> <td bgcolor=\"#ffffff\"><a  href=\"$html\">$desc</a></td> </tr>\n";
        }
    } 
    

     $default = $default .   "  </tr>";
               
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




