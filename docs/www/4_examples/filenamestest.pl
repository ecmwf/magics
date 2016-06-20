#!/usr/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


use common::mpp_utils;
use File::stat;
use Cwd;                # for the function 'getcwd'

my $ecmwf       = file_to_string         ("common/ecmwf.htmlx");
my @cat_menu    = testsuite_submenus (); 
my $page_title  = "Magics++ Filename Test Suite";
my $progs_dir   = "4_examples/progs";



# ------------------------------------------------------------------------
# magics_version_from_dir
# Returns the name of the magics version from the current directory
# (assumes a certain directory structure).
# ------------------------------------------------------------------------

sub magics_version_from_dir {

    local ($filename)  = $_[0];
    local ($desc)      = $_[1];
    local @aparts;
    
    my    $strResult;
    my    $strDir;


    # get a listing of the program

    $strDir = `pwd`;


    # split into individual words and store in array

    @aparts = split (/\//, $strDir);


    $strResult = @aparts[$aparts - 3];

    return $strResult;
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
    $default = "<title>" . $page_title . " ($platform)" . "</title>";
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
    $default = $page_title .  " ($platform)";
    return $default;
}
 
sub submenu {
    my ($default) = @_;
   
    return $default;
}

sub content {
    my ($default) = @_;

    my $source     = "test_filenames";
    my $srcfile    = "$progs_dir/c/filenames/$source.c";
    my $exe_mnew   = "$progs_dir/bin/$source" ."_mpp";
    my $results    = "$progs_dir/logs/test_filenames_out.log";
    my $results_text = log_file_to_string ($results);
    my $srccode      = c_file_to_string   ($srcfile);

    my $program_details     = program_listing       ($exe_mnew);
    my $lastmod_details     = last_modified_details ($exe_mnew);
    my $lastlogmod_details  = last_modified_details ($results);
    my $exesizenew           = filesize ("$exe_mnew");

    my $test_suite_selection = test_suite_selection_html("Filename");


    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<P><P>"
               . "<tbody>" 
               . "  <tr>" 
                . "    <td  class=\"menuheading\">$desc</td>"
               . "  </tr>" 
               . "  <tr>" 
                . "    <td   bgcolor=\"#ffffff\">
    <center>
    $test_suite_selection
    <P>
    <table border = \"1\" bgcolor = \"#EEEEFF\">
    <tr>
        <td> Magics++ executable generated: </td>
        <td> <b>$lastmod_details, $exesizenew</b> </td>
        <td> </td>
    </tr>
    <tr>
        <td> Magics++ results file generated: </td>
        <td> <b>$lastlogmod_details</b> </td>
        <td> </td>
    </tr>
    </table>
    </center>
    <pre>$results_text</pre>
    <hr>
    <p>
	<pre class=\"Source\">$srccode
    </pre>
    ";



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



$html = "html/test/filenames/c";
mkdir ($html);
$html = "$html/filenames.html";
$/ = undef;
open(IN,file_to_string("common/template.htmlx"));
my $text = <IN>;
close(IN);
$text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

open  HTML, ">" . $html;
print HTML $text;
close HTML;

print ("Writing to $html\n");
