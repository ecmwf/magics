#!/usr/bin/env perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# ----------------------------------------------------------------------
# Command-line options:
#   <expt-dir> [debug-level]
#      debug-level:
#      This sets the level of debug information that is output.
#      (0 - none; 1 - important only; 2 - detailed, 3 - very detailed)
# ----------------------------------------------------------------------


use strict;
use File::stat;
my %info;
my $Author="Sylvie Lamy-Thepaut / Iain Russell";
my $file;
my @tests;
my $test;
my $ecmwf="http://www.ecmwf.int";

# check for command-line options

my $debug = @ARGV[1];


# open and read the directory of experiments

debug_print ("Experiments directory: @ARGV[0]\n", 1);
opendir DIR, @ARGV[0]  or die "Cannot open @ARGV[0]";

my @files=readdir(DIR);
debug_print ("Experiment directory: @files\n", 1);

foreach $file (@files) 
{
	next if ($file =~ /^\.(.)*/);
	push(@tests, $file);
}



# for each experiment, generate the HTML

foreach $test  (@tests)
{
	my @exps = split("_", $test);
	debug_print ("  exps = @exps\n", 2);
	my $exp1 =  "$exps[0]-$exps[1]-$exps[2]";
	my $exp2 =  "$exps[3]-$exps[4]-$exps[5]";
	debug_print ("  exp1 = $exp1\n", 2);
	debug_print ("  exp2 = $exp2\n", 2);


open HTML, ">html/$test.html";

print HTML <<"HTML";

 
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html><!-- #BeginTemplate "/Templates/ecmwf.dwt" -->
<head profile="http://purl.org/metadata/dublin_core">
<!-- ECMWF: Standard External Template -->
<!-- ECMWF: Version 1.03 -->
<META http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<META content="$Author: unknown $" name="DC.Creator" />
<!-- #BeginEditable "meta" --> 
<!-- WEBMARS meta -->
<!-- #EndEditable --> <!-- #BeginEditable "metalink" --> 
<!-- WEBMARS metalink -->
<!-- #EndEditable --> <!-- #BeginEditable "doctitle" --> 
<title> 
HTML

print HTML "Magics Test: $test\n";

print HTML <<"HTML";
</title>
<!-- #EndEditable --> <!-- #BeginEditable "customstyle" --> 
<!-- WEBMARS customstyle -->
<!-- #EndEditable --> 
<script type="text/javascript" language="JavaScript" src="$ecmwf/assets/js/ecmwf.js">
</script>
<link rel="stylesheet" href="$ecmwf/assets/css/ecmwf.css" type="text/css" />
</head>
<body bgcolor="#FFFFF2" text="#000000">
<a name="top"></a> 
<table summary="Formatting Table: Page Layout" width="750" border="0" cellspacing="0" cellpadding="0" bgcolor="#FFFFF2">
  <tr> 
    <td bgcolor="#CCCE9B" align="left" valign="middle"><a href="$ecmwf/"><img src="$ecmwf/assets/images/text_logo.jpg" border="0" alt="Home page" width="136" height="26" align="middle" /></a></td>
    <td bgcolor="#CCCE9B">&nbsp;</td>
    <td bgcolor="#CCCE9B" valign="middle"> 
      <form name="search" method="post" action="/cgi-bin/htsearch">
        <input type="hidden" name="restrict" value="" />
        <table summary="Formatting Table: Actions Menu" border="0" cellspacing="4" cellpadding="0" align="right">
          <tr> 
            <td><a href="$ecmwf/" class="menuitem">Home</a></td>
            <td>&nbsp;</td>
            <td><a href="$ecmwf/your/d/room/" class="menuitem">Your Room</a></td>
            <td>&nbsp;</td>
            <td><a href="$ecmwf/tools/login/" class="menuitem">Login</a></td>
            <td>&nbsp;</td>
            <td><a href="$ecmwf/about/contact/" class="menuitem">Contact</a></td>
            <td>&nbsp;</td>
            <td><a href="$ecmwf/about/feedback/" class="menuitem">Feedback</a></td>
            <td>&nbsp;</td>
            <td><a href="$ecmwf/about/sitemap/" class="menuitem">Site Map</a></td>
            <td>&nbsp;</td>
            <td><a href="$ecmwf/tools/qsearch/" class="menuitem">Search:</a></td>
            <td valign="bottom"> 
              <input type="hidden" name="exclude"  value="" />
              <input tyte="hidden" name="config"   value="" />
              <input type="text" name="words" maxlength="128" size="8" style="height:20px; width:80px" />
            </td>
            <td>&nbsp;</td>
          </tr>
        </table>
      </form>
    </td>
  </tr>
  <tr> 
    <td align="center" valign="middle" width="136" bgcolor="#EEEEDE"><a href="$ecmwf/samples/d/banner/page.html"><img alt="Discover this product" border="0" width="136" height="64" src="$ecmwf/samples/d/banner/image.jpg" /></a></td>
    <td bgcolor="#EEEEDE" >&nbsp;</td>
    <td align="right" bgcolor="#EEEEDE" > 
      <table summary="Formatting Table: Top Navigation" cellpadding="2" cellspacing="0" border="0" width="100%" >
        <tr bgcolor="#EEEEDE"> 
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="$ecmwf/about/"><span class="topnav">About Us</span></a><br />
            <a class="topnav1" href="$ecmwf/about/overview/">Overview</a><br />
            <a class="topnav1" href="$ecmwf/about/location/">Getting here</a><br />
            <a class="topnav1" href="$ecmwf/about/committees/">Committees</a> 
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="$ecmwf/products/"><span class="topnav">Products</span></a><br />
            <a class="topnav1" href="$ecmwf/products/forecasts/">Forecasts</a><br />
            <a class="topnav1" href="$ecmwf/products/data/">Order Data</a><br />
            <a class="topnav1" href="$ecmwf/products/data/software/">Order Software</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="$ecmwf/services/"><span class="topnav">Services</span></a><br />
            <a class="topnav1" href="$ecmwf/services/computing/">Computing</a><br />
            <a class="topnav1" href="$ecmwf/services/archive/">Archive</a><br />
            <a class="topnav1" href="$ecmwf/services/prepifs/">PrepIFS</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="$ecmwf/research/"><span class="topnav">Research</span></a><br />
            <a class="topnav1" href="$ecmwf/research/ifs/">Modelling</a><br />
            <a class="topnav1" href="$ecmwf/research/era/">Reanalysis</a><br />
            <a class="topnav1" href="$ecmwf/research/seasonal/">Seasonal</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="16%"> <a href="$ecmwf/publications/"><span class="topnav">Publications</span></a><br />
            <a class="topnav1" href="$ecmwf/publications/newsletters/">Newsletters</a><br />
            <a class="topnav1" href="$ecmwf/publications/manuals/">Manuals</a><br />
            <a class="topnav1" href="$ecmwf/publications/library/">Library</a><br />
          </td>
          <td valign="top" bgcolor="#EEEEDE" width="20%"> <a href="$ecmwf/newsevents/"><span class="topnav">News&amp;Events</span></a><br />
            <a class="topnav1" href="$ecmwf/newsevents/calendar/">Calendar</a><br />
            <a class="topnav1" href="$ecmwf/newsevents/employment/">Employment</a><a class="topnav1" href="$ecmwf/newsevents/calendar/"></a><br />
            <a class="topnav1" href="$ecmwf/newsevents/itt/">Open Tenders</a><br />
          </td>
        </tr>
      </table>
    </td>
  </tr>
  <tr> 
    <td  align="left" valign="middle" width="136" bgcolor="#CCCE9B"><!-- #BeginEditable "spare1"
	--><!-- WEBMARS spare1 --><!-- #EndEditable -->&nbsp;</td>
    <td bgcolor="#CCCE9B" >&nbsp;</td>
    <td valign="middle" bgcolor="#CCCE9B" > 
      <table summary="Formatting Table: Location header" cellpadding="0" cellspacing="0" border="0" width="100%">
        <tr> 
          <td align="left"><span class="location"><!-- #BeginEditable "location" --> 
            <!-- WEBMARS location -->
            <!-- #EndEditable -->&nbsp;</span> 
          </td>
          <td align="right"> <!-- #BeginEditable "topprevnext" --> 
            <!-- WEBMARS topprevnext -->
            <!-- #EndEditable --> 
          </td>
        </tr>
      </table>
    </td>
  </tr>
  <tr> 
    <td align="left"><!-- #BeginEditable "spare2" -->
      <!-- WEBMARS spare2-->
      &nbsp;<!-- #EndEditable --> 
    </td>
    <td>&nbsp;</td>
    <td> 
      <!-- #BeginEditable "heading" --> 
        <!-- WEBMARS heading -->
        <!-- #EndEditable -->&nbsp;
    </td>
  </tr>
  <tr> 
    <td valign="top" width="136" background="/assets/images/map_backdrop.jpg"> 
      <!-- #BeginEditable "submenu" --> 
      <!-- WEBMARS submenu-->
<table background="/assets/shim.gif" bgcolor="#FFF1B3" border="0"
cellpadding="1" cellspacing="0" width="100%" class="submenu">
        <tbody> 
        <tr> 
          <td><a class="menuheading" href="index.html">Test Report</a></td>
        </tr>
        <tr> 
          <td> 
            <table bgcolor="#ffffff" border="0" cellpadding="3" cellspacing="0"
width="100%">
              <tr> 
                <td> 
                  
            <table bgcolor="#ffffff" width="100%">
HTML

foreach $test (@tests) 
{
    # make some abbreviations in the test names so that this sidebar
    # does not take up so much room...

    my $test_abbr = $test;

    $test_abbr =~ s/double/dbl/g;
    $test_abbr =~ s/single/sgl/g;
    $test_abbr =~ s/cylindrical/cyl/g;
    $test_abbr =~ s/polar/ps/g;

    print HTML "         <tr> <td><a class=\"menuitem\" href=\"$test.html\">$test_abbr</a></td> </tr>\n";
}

my $output = "$test.html";
print HTML <<"HTML";
            </table>
                </td>
              </tr>
            </table>
          </td>
        </tr>
        </tbody> 
      </table>
    </td>
	<td> &nbsp; </td>
    <td  valign="top"> 
      <!-- #BeginEditable "content" --> 
      <!-- WEBMARS content -->
<table background="/assets/shim.gif" bgcolor="#FFF1B3" border="0"
cellpadding="1" cellspacing="0" width="100%" class="submenu">
        <tbody> 
        <tr> 
          <td  class="menuheading"> Test Report : <a href=\"$output\" > $test </a> </td>
        </tr>
        <tr> 
          <td> 
<table background="/assets/shim.gif" bgcolor="#FFF1B3" border="0"
cellpadding="1" cellspacing="1" width="100%">
        <tbody> 
              <tr> 
                <td bgcolor="#ffffff"> <b> File size (ps1-ps2) </b> </td> 
                <td bgcolor="#ffffff"> <b> Diff size </b> </td> 
                <td bgcolor="#ffffff"> <b> Diff Postscript </b> </td> 
				<td bgcolor="#ffffff"> <b> $exp1 </b></td>
				<td bgcolor="#ffffff"> <b> $exp2 </b></td>
              </tr> 
HTML
	my $ps_sourcedir ="@ARGV[0]/$test/ps_source";
	my $ps_diffdir   ="@ARGV[0]/$test/ps_diff";
	my %info;
open IN, "@ARGV[0]/$test/report" or die "Could not open @ARGV[0]/$test/report";
while(<IN>)
{
    my ($ps1, $x, $ps2, $x, $diff) = split();

    $diff=0 unless $diff;
    next if ($ps1 eq "open");
	my $ps=$ps1;
	$ps=~s/exp1_//;
	my $nb=$diff;
	$nb=~s/\(//;
	$nb=~s/\)//;
	$info{$ps}=$nb;

}
close IN;
	opendir DIR, $ps_sourcedir or die " canot open $ps_sourcedir";
	my @files=readdir(DIR);
	foreach $file (@files) 
	{
          
          next unless ($file =~ /exp1(.)/);
          #next unless ($file =~ /(.)diff.ps/);
         
		  my $ps=$file;
		  $ps=~s/exp1_//;
		  my $stat = stat("$ps_sourcedir/$file");
		  my $nb = $info{$ps}; 
		  my $color="black";
		  $color="red" if ($nb > 15000);
		  $color="green" if ($nb < 5000);
          my $code = 0;
		  my $stat1;
          $stat1 = stat("$ps_sourcedir/exp1_$ps") or $stat1 = 0;
		  my $stat2;
          $stat2 = stat("$ps_sourcedir/exp2_$ps") or $stat2 = 0;
          if ($stat1 eq 0) {
            my $link = "file://$test";
            print  HTML  "<tr>\n";
            printf HTML  "<td bgcolor=\"#ffffff\"> <Font color=\"red\" >???  </Font></td>\n"; 
            printf HTML  "<td bgcolor=\"#ffffff\"> <Font color=\"red\" > %s </Font> </td>\n", $nb; 
            printf HTML  "<td bgcolor=\"#ffffff\"> <a href=\"../$ps_diffdir/$ps.diff.ps\"> $ps </a> </td>\n"; 
            print  HTML  "<td bgcolor=\"#ffffff\"> <Font color=\"red\" > $ps not found! </Font> </td>\n"; 
            print  HTML  "<td bgcolor=\"#ffffff\"> <a href=\"exp2_$ps\"> $ps </a> </td>\n"; 
            print  HTML  "</tr>\n";
            $code = 1;
          }
          
          if ($stat2 eq 0) {
              
            my $link = "file://$test";
            print  HTML  "<tr>\n";
            printf HTML  "<td bgcolor=\"#ffffff\"> <Font color=\"red\" > ??? </Font></td>\n"; 
            printf HTML  "<td bgcolor=\"#ffffff\"> <Font color=\"$color\" > %s </Font> </td>\n", $nb; 
            printf HTML  "<td bgcolor=\"#ffffff\"> <a href=\"../$ps_diffdir/$ps.diff.ps\"> $ps </a> </td>\n"; 
            print  HTML  "<td bgcolor=\"#ffffff\"> <a href=\"../$ps_sourcedir/exp1_$ps\">  $ps </a> </td>\n"; 
            print  HTML  "<td bgcolor=\"#ffffff\"> <Font color=\"red\" > $ps : not found!  </Font> </td>\n"; 
            print  HTML  "</tr>\n";
            $code =1;
          }
          if ($code eq 0) {
            my $diff =  ($stat1->size) - ($stat2->size);
		    my $link = "file://$test";
            print  HTML  "<tr>\n";
            printf HTML  "<td bgcolor=\"#ffffff\"> %s  </td>\n", $diff; 
            printf HTML  "<td bgcolor=\"#ffffff\" text=\"$color\" color=\"$color\"> <Font color=\"$color\" > %s </Font> </td>\n", $nb; 
            printf HTML  "<td bgcolor=\"#ffffff\"> <a href=\"../$ps_diffdir/$ps.diff.ps\"> $ps </a> </td>\n"; 
            print  HTML  "<td bgcolor=\"#ffffff\"> <a href=\"../$ps_sourcedir/exp1_$ps\">  $ps </a> </td>\n"; 
            print  HTML  "<td bgcolor=\"#ffffff\"> <a href=\"../$ps_sourcedir/exp2_$ps\">  $ps </a> </td>\n"; 
            print  HTML  "</tr>\n";
          }
	}

print HTML <<"HTML";
        </tbody> 
            </table>
          </td>
        </tr>
        </tbody> 
      </table>
      <!-- #EndEditable --> <br />
      <!-- #BeginEditable "related" --> 
      <!-- WEBMARS related -->
    </td>
  </tr>
  <tr> 
    <td  bgcolor="#CCCE9B" valign="baseline"> 
      <table summary="Formatting Table: date footer" height="16" border="0" cellspacing="0" cellpadding="0">
        <tr> 
          <td><a href="#top"><img src="$ecmwf/assets/icons/up.gif" alt="Top of page" border="0" width="18" height="18" /></a></td>
          <td><span class="documentdate"><!-- #BeginEditable "version" --> 
            <!--WEBMARS version -->
            <!-- #EndEditable --></span></td>
        </tr>
      </table>
    </td>
    <td valign="middle" bgcolor="#CCCE9B">&nbsp;</td>
    <td  bgcolor="#CCCE9B" align="right" valign="baseline"> 
      <table summary="Formatting table: main footer" width="100%" border="0" cellspacing="0" cellpadding="0">
        <tr align="right"> 
          <td align="left"><span class="info"><!-- #BeginEditable "info" --> 
            <!--WEBMARS info -->
            <!-- #EndEditable --></span></td>
          <td bgcolor="#CCCE9B"><span class="editor"><a href="$ecmwf/tools/detailer/"><img alt="Page Details" src="$ecmwf/assets/icons/detailer.gif" border="0" width="18" height="18"></a><!-- #BeginEditable "editor" --> 
            <!--WEBMARS editor -->
            <!-- #EndEditable --></span></td>
          <td bgcolor="#CCCE9B"><span class="copyright"><a href="$ecmwf/frontpage/copyright/">&copy; 
            ECMWF</a> </span></td>
          <td><!-- #BeginEditable "botprevnext" --> 
            <!-- WEBMARS botprevnext -->
            <!-- #EndEditable --></td>
        </tr>
      </table>
    </td>
  </tr>
  <tr> 
    <td height="1" width="136" bgcolor="#CCCB9E"><img alt="shim" height="1" width="136" src="$ecmwf/assets/shim.gif" /></td>
    <td height="1" width="008" bgcolor="#CCCB9E"><img alt="shim" height="1" width="8" src="$ecmwf/assets/shim.gif" /></td>
    <td height="1" width="100%" bgcolor="#CCCB9E"><img alt="shim" height="1" width="160" src="$ecmwf/assets/shim.gif" /></td>
  </tr>
</table>
</body>
<!-- #EndTemplate --></html>
HTML
}








# ******************************************************************
# Subroutine : debug_print
# Prints the given string if debug is on
# Parameter 1 ($_[0]): text to print
# Parameter 2 ($_[1]): debug level to print it at
# ******************************************************************

sub debug_print
{
	my $text  = @_[0];
	my $level = @_[1];

	# Are we at a high enough debug level to print this text?

	if ($debug >= $level)
	{
		print ("$text");
	}
}

# End of subroutine 'debug_print'


