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
use File::Basename;

my $ecmwf       = file_to_string ("common/ecmwf.htmlx");
my @menu        = epsgram_testsuite_menu (); 
my @devices     = epsgram_devices ();
my $max_pages   = 15;
my $lib_version = 'MAGPLUSLIB_SHARED';
my $page_title  = "Magics++ EPSgram Test Suite";



# ---------------------------------------
#  filesize
#  Returns the size of a file as a string
# ---------------------------------------

sub filesize {

    local($filename)  = $_[0];
    my $size = -1024;
    my $postfix;
    my $rounded;

    $st = stat($filename);
    
    if ($st)
    {
        $size = $st->size;
    }

    $size = $size / 1024;  # convert to Kb
    $rounded = sprintf("%.0f", $size);
    $postfix = "K";

    if ($size > 1024)      # convert to Mb?
    {
        $size = $size / 1024;
        $postfix = "Mb";
        $rounded = sprintf("%.2f", $size);
    }
    

    return "$rounded $postfix";
}


# ---------------------------------------------
# magics_lib
# Returns a listing of the MAGPLUSLIB directory
# ---------------------------------------------

sub magics_lib {

    local $magpluslibenv = $ENV{$lib_version}  || return ('Could not retrieve $lib_version.');
    local $magpluslib;
    local @alibs;
    local $strProg = "<PRE>";
    local $strResult = "<PRE>";


    # get the version of the EPSgram executable

    $strProg   = `which metgram`;
    $strResult = $strResult . $strProg . "\n";
    $strResult = $strResult . `ls -lh $strProg` . "\n";


    # split into individual words and store in array

    @alibs = split (/ /, $magpluslibenv);


    # we want the first string

    $magpluslib = @alibs [0];

    $magpluslib = substr ($magpluslib, 2, length($magpluslib) - 2);


    # get a directory listing

    $strResult = $strResult . `ls -lh $magpluslib`;

    $strResult = $strResult . "</PRE>";

    return $strResult;
}




# ------------------------------------------------------------------------
# program_listing
# Returns a listing of the program executable (for the date/time and size)
# ------------------------------------------------------------------------

sub program_listing {

    local ($filename)  = $_[0];
    
    my    $strResult = "<PRE>";


    # get a listing of the program

    $strResult = $strResult . `ls -lh $filename`;

    $strResult = $strResult . "</PRE>";

    return $strResult;
}



# ------------------------------------------------------------------------
# last_modified_details
# Returns the last-modified details for the program executable
# ------------------------------------------------------------------------

sub last_modified_details {

    local ($filename)  = $_[0];
    local ($desc)      = $_[1];
    local @aparts;
    
    my    $strResult = "";
    my    $strListing;


    if (-e $filename)
    {

        # get a listing of the program

        $strListing = `ls -lh $filename`;


        # split into individual words and store in array

        @aparts = split (/\s+/, $strListing);


        $strResult = $strResult . @aparts[5] . " " . @aparts[6] . " " . @aparts[7];
    }

    else
    {
        $strResult = "File $filename does not exist";
    }

    return $strResult;
}





# ------------------------------------------------------------------------
# check_for_error_messages
# Checks the given log files for error messages. Returns 1 (error) or 0
# ------------------------------------------------------------------------

sub check_for_error_messages {

    local($filename) = @_[0];
    local($line)   = ""; 

    open (FILE, $filename) || return 0;  # assume no news is good news...


    @Lines = <FILE>;
    foreach $line (@Lines)
    {
        if ($line =~ /ERROR/)
        {
            return 1;
        }
    }

    return 0;    # if we got to here, then there were no error messages
}





# ------------------------------------------------------------------------
# parse_timing_text
# Returns a hash containing the relevant parts of the timing text
# Note that this depends on the output of the ksh 'time' command
# - which has changed since the new ksh was installed (Jan 2006).
# ------------------------------------------------------------------------

sub parse_timing_text {

    local ($strText)  = $_[0];
    local @aParts;
    

    # split into individual words and store in array

    @aParts = split (/\s+/, $strText);
    
    
    # is the time ok?
    
    if (@aParts[0] eq "cannot")
    {
        $hTimes{"ok"}     = "n";
        $hTimes{"string"} = "No timing file";
    }

    else
    {
        if ($strText =~ /Abort/)        # the string 'Abort' appears...
        {
            $hTimes{"ok"}     = "n";
            $hTimes{"string"} = "Abort";
        }

        else 
        {
            if ($strText =~ /Memory/)   # the string 'Memory' appears...
            {
                $hTimes{"ok"}     = "n";
                $hTimes{"string"} = "Mem fault";
            }
            else                        # seems ok
            {
                $hTimes{"ok"}     = "y";
                $hTimes{"real"}   = @aParts[2];
                $hTimes{"user"}   = @aParts[4];
                $hTimes{"system"} = @aParts[6];
                $hTimes{"string"} = "real: $hTimes{\"real\"}, user: $hTimes{\"user\"}, sys: $hTimes{\"system\"}";
            }
        }
    }

    return $hTimes;
}





# ---------------------------------------------------------------
# generate_timings_table
# Creates an html table full of all the available program timings
# ---------------------------------------------------------------

sub generate_timings_table {

    my $progs_dir  = "4_examples/progs";
    my $htmldir    = "html/test/epsgram";


    # table with the test suite summary

    my $summary_text = file_to_string_nofail ("4_examples/progs/logs/epsgram_summary.txt");
    my $summary = "\n<table border=\"1\" bgcolor=\"#FFEEEE\">\n";
    
    $summary = $summary ."
        <tr>
            <td align=\"center\"><B>metgram Test Suite Settings Summary</B></td>
        </tr>
        <tr>
            <td>
                <pre>$summary_text</pre>
            </td>
        </tr>
        </table>
    ";
    


    # table headings etc

    my $table = "\n<table border=\"1\">\n";

    $table = $table . "<tr>";
    $table = $table . "<th>Program";
    foreach my $device (@devices)
    {
        $table = $table . "<th><b>$device</b></td>";
    }
    $table = $table . "</tr>\n";



    # the body of the table - one line per program

    foreach my $example (@menu)
    {
        # write a new line of timings
        
        $source = $example->{source};
        $table = $table . "<tr>";
        $table = $table . "<td>$example->{name}</td>";
    
        foreach my $device (@devices)
        {
            my $time_file = "$htmldir/$source.time";
	    print ("TIME: $time_file\n");
            my $time_text = file_to_string_nofail($time_file);
            my $hTimes    = parse_timing_text($time_text);
            my $bgcolour  = $proj_info{"CYLINDRICAL"}{"bgcolour"};
            my $time      = "";

            if ($hTimes{"ok"} eq "y")
            {
                $time = $hTimes{"user"};
            }
            else
            {
                $time = "2"; # "<b>" . $hTimes{"string"} . "</b>";
            }

            $table = $table . "<td bgcolor=\"$bgcolour\">$time</td>";
        }

        $table = $table . "</tr>\n";
    }
    
    
    # finish off the table
    
    $table = $table . "</table>";
    
    
    # put it all together
    
    my $page_content = $summary . "\n<p>\n" . $table . "\n<p>\n";
    
    return $page_content;
}



# ---------------------------------------------------------------
# generate_output_table_row
# Creates an html table with the output files
# ---------------------------------------------------------------

sub generate_output_table_row {

    local ($strSource)           = $_[0];
    local ($strOutputExtension)  = $_[1];
    local ($strOutputNumber)     = $_[2];
    local ($strSourceFile)       = $_[3];
    local ($strOutputDir)        = $_[4];
    local ($strDoMeta)           = $_[5];
    local ($strFileTypeName)     = $strOutputExtension . "file";
    local ($strResult)           = "";
    local ($strRowImages)        = "";
    local ($strRowDetails)       = "";
    local (@astrFiles);
    local ($fname);
    local ($thumbname);
    local ($lastmod);
    local ($filesize);
#    local ($time);
    local ($metafile);
    local ($metalink);


    $strRowImages  = $strRowImages  . "<tr>";
    $strRowDetails = $strRowDetails . "<tr>";



#    $time_file    = basename ($strSourceFile, ".xml") . ".time";
#    $time = file_to_string_nofail   ("$strOutputDir/$time_file");



    # create a table cell giving the name of file type for this row

    $strRowImages  = $strRowImages  . "<td>" . uc($strOutputExtension) . "<br>" . "</td>";
    $strRowDetails = $strRowDetails . "<td>" . "</td>";


    # do some fancy regular expression parsing of the source XML file
    # to extract the names of the desired output files
    
    $_ = $strSource;
    ($first, $second, $third) = /$strFileTypeName.*\'(.*)\.$strOutputExtension\'/g;
#    ($first, $second, $third) = /$strFileTypeName=\'([^\'])*'/g;
#    ($first, $second, $third)= /$strFileTypeName='([\w|-|\.^']*)'.*/g;

#    print ("-----" . $strSource . "\n");
#    print ("Files  ($strFileTypeName): $first, $second, $third\n");

    # put the names into a list and create a table element for each

    @astrFiles = ($first, $second, $third);

    foreach $file (@astrFiles)
    {
        if ($file ne "")
        {
            if ($strOutputExtension eq "gif")
            {
                $fname        = $file  . "." . $strOutputNumber;
                $thumbname    = $fname . "_small" . ".gif";
                $fname        = $fname . "." . $strOutputExtension;
            }
            
            else
            {
                $fname        = $file  . $strOutputNumber;
                $thumbname    = $fname . "_small" . ".gif";
                $fname        = $fname . "." . $strOutputExtension;
            }

            $strRowImages = $strRowImages . "<td><a href=\"$fname\"><image src=\"$thumbname\"></a></td>";

            $lastmod  = last_modified_details ("$strOutputDir/$fname");
            $filesize = filesize              ("$strOutputDir/$fname");

            if ($strDoMeta eq "dometa")
            {
                $metafile = basename ($fname, ".$strOutputExtension")  . ".meta";
                $metalink = "<br><a href=\"$metafile\">Meta file</a>";
            }

            $strRowDetails = $strRowDetails . "<td>Updated: $lastmod<br>$filesize$metalink</td>";
        }
    }


    $strRowImages  = $strRowImages  . "</tr>";
    $strRowDetails = $strRowDetails . "</tr>";

    $strResult = $strRowImages . $strRowDetails;
    
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
   

    # create the menu of categories

    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<tbody>" 
               . "<tr>"
               . "  <td><span class=\"menuheading\"><font color=\"#000000\">Categories</font></span></td>"
               . "</tr>"
               . "<tr>" 
               . "  <td> "
               . "    <table bgcolor=\"#ffffff\" border=\"0\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">"
               . "      <tr>" 
               . "        <td>" 
               . "            <table bgcolor=\"#ffffff\" width=\"100%\">";


    # add a link to the timings page

    $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"timings.html\"><b><i>Timings</i></b></a></td> </tr>\n";


    # add links to each category

    foreach my $item (@cat_menu)
    {
        my $title  = $item->{name};
        my $source = $item->{first};
        
        $html = $source . ".html";
        
        $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$html\"><b>$title</b></a></td> </tr>\n";
    }

    $default = $default . "</table>"
                . "        </table>"
                . "    </td>"
                . "   </tr>"
                . "   </table>";



    # create the menu of programs available in this category


    $default = $default
               . "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<tbody>" 
               . "<tr>"
               . "  <td><span class=\"menuheading\"><font color=\"#000000\">Programs</font></span></td>"
               . "</tr>"
               . "<tr>" 
               . "  <td> "
               . "    <table bgcolor=\"#ffffff\" border=\"0\" cellpadding=\"3\" cellspacing=\"0\" width=\"100%\">"
               . "      <tr>" 
               . "        <td>" 
               . "            <table bgcolor=\"#ffffff\" width=\"100%\">";






    foreach my $item (@menu)
    {
        my $title  = $item->{name};    
        my $html   = $item->{html};
        my $source = $item->{source};
        
        if ($item->{cat} eq $cat)
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

    # if generating the timings page, we use a specific function for that

    if ($timings)
    {
        return $default . generate_timings_table ();
    }


    my $htmldir    = "html/test/epsgram";
    my $progs_dir  = "4_examples/progs";
    my $srcdir     = "$progs_dir/epsgram/test";
    my $srcfile    = "$srcdir/$source.epsml";
    my $notesfile  = "$srcdir/$source.txt";
#    my $vlog_srcn  = "$progs_dir/logs/$source.vlog";
    my $time       = "$htmldir/$source.time";
    my $notes_table = "";
    my $proj_selection = "";
    my $dev_selection  = "";
    my $srccode       = magml_file_to_string ($srcfile);
    my $srcplain      = file_to_string ($srcfile);
    my $template_src  = eps_template_from_magml_to_string ($srcfile, $srcdir);
    my $pic_mnew_thumb    = $source . "_small.gif";
    my $ps_mnew           = "$source";
    my $pdf_mnew          = "$source.pdf";
    my $log_mnew          = "$source.log";
    my $err_mnew          = "$source.err";
#    my $vlog_mnew         = "$source.vlog";

    my $filesizenew    = filesize ("$htmldir/$ps_mnew");
    my $pdfsizenew     = filesize ("$htmldir/$pdf_mnew");
    
    my $magml_projection = "";

    my $test_suite_selection = test_suite_selection_html("EPSgram");
    
    print ("File: $srcfile\n");
    my $ps_row  = generate_output_table_row ($srcplain, "ps",  "",  $srcfile, $htmldir, "dometa");
    my $gif_row = generate_output_table_row ($srcplain, "gif", "1", $srcfile, $htmldir, "nometa");


    # which projection is used in the MagML example? Use regexp to find!
    
    if ($srccode =~ /polar_stereographic/i)
    {
        $magml_projection = "POLAR_STEREOGRAPHIC";
    }
    else
    {
        $magml_projection = "CYLINDRICAL";
    }

    
   

    if (-e $notesfile)
    {
        my $notes  = file_to_string( $notesfile);
        
        $notes_table = "
        <P>
        <TABLE border = \"1\" bgcolor = \"#EEEEEE\">
        <TR>
            <TD align=\"center\"><B>Notes</B></TD>
        </TR>
        <TR>
            <TD>
                $notes
            </TD>
        </TR>
        </TABLE>
        <P>
        "
    }


    my $magplusdir         = magics_lib ();
    my $lastps_details     = last_modified_details   ("$htmldir/$ps_mnew");
#    my $lastvlog_details   = last_modified_details   ("$vlog_srcn");
    my $time_text       = file_to_string_nofail   ($time);
    

    


    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<P><P>"
               . "<tbody>" 
               . "  <tr>" 
                . "    <td  class=\"menuheading\">$desc</td>"
               . "  </tr>" 
               . "  <tr>" 
                . "    <td   bgcolor=\"#ffffff\">
    <CENTER>
    $test_suite_selection
    <P>
    <table>
    $ps_row
    $gif_row
    </table>
    <TABLE border=\"1\">
    <TR>
        <TD>Time: $time_text<BR></TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <A HREF=\"$log_mnew\">Log file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"err_icon.gif\" ALT=\"MAGICS++ Error\"> <A HREF=\"$err_mnew\">Error file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"valgrind.gif\" ALT=\"MAGICS++ Valgrind Output\"> <A HREF=\"$vlog_mnew\">Valgrind output</A>
             (generated: $lastvlog_details)</TD>
    </TR>
    </TABLE>
    $notes_table
    </CENTER>
    <HR>
    <P>
    <TABLE border = \"1\" bgcolor = \"#FFFFEE\">
    <TR>
        <TD align=\"center\"><B>MagML_Interpreter and Magics++ library directory (from \$$lib_version)</B></TD>
    </TR>
    <TR>
        <TD>
            $magplusdir
        </TD>
    </TR>
    </TABLE>
    <P>
    <b>Source:</b><p>
    <pre class=\"Source\">
$srccode
    </pre>
    <b>Template:</b><p>
    <pre class=\"Source\">
$template_src
     </pre>
                       </td>"
               . "  <tr>" 
               . " <td   bgcolor=\"#ffffff\"> &nbsp; </td>"
               . "  </tr>" 
               . "  </tr>";
                   

    $default = $default . "
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








$timings = 0;  # no, we're not generating the timings page yet


# -------------------------------------------------------
# Main loop - for each example program, generate the html
# -------------------------------------------------------

foreach my $example (@menu)
{
    my $title  = $example->{name};    
    my $html   = "";
       $source = $example->{source};
       $desc   = $example->{description};
       $no69   = $example->{no69};
    my $lang   = $example->{lang};
       $cat    = $example->{cat};

    $html = "html/test/epsgram/$source.html";
    $/ = undef;
    open(IN,file_to_string("common/template.htmlx"));
    my $text = <IN>;
    close(IN);
    $text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

    open HTML, ">" . $html;
    print HTML $text;
    close HTML;
}



# ------------------------------------------------------------
# Now we want to generate a page that contains all the timings
# ------------------------------------------------------------

$timings = 1;  # we are generating the timings page
$projection = @projections[0];

$timingshtml = "html/test/epsgram/timings.html";

$/ = undef;
open(IN,file_to_string("common/template.htmlx"));
my $text = <IN>;
close(IN);
$text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

open HTML, ">" . $timingshtml;
print HTML $text;
close HTML;


