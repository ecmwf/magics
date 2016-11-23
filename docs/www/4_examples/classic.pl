#!/usr/bin/env perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


use common::mpp_utils;
use File::stat;

my $ecmwf       = file_to_string ("common/ecmwf.htmlx");
my @menu        = classic_menu   (); 
my @cat_menu    = classicsuite_submenus (); 
my @projections = ("CYLINDRICAL");
my @devices     = devices        ();
my $max_pages   = 10;
my $page_title  = "Classic Magics Test Suite";

my $magplusdir  = magics_lib ();
my $psdir      = "html/classic/fortran";
my $progs_dir  = "4_examples/progs";


# ---------------------------------------------
# magics_lib
# Returns a listing of the MAGPLUSLIB directory
# ---------------------------------------------

sub magics_lib {

    local $magpluslibenv = $ENV{$mpplibname}  || return ("Could not retrieve $mpplibname.");
    local $magpluslib;
    local @alibs;
    my    $strResult = "<PRE>";


    # split into individual words and store in array

    @alibs = split (/ /, $magpluslibenv);


    # we want the first string

    $magpluslib = @alibs [0];


    # but if shared, then we want to remove the '-L' from the front 

    if (substr ($magpluslib, 0, 1) eq '-')
    {
        $magpluslib = substr ($magpluslib, 2, length($magpluslib) - 2);
    }


    # get a directory listing

    $strResult = $strResult . `ls -lh $magpluslib`;

    $strResult = $strResult . "</PRE>";

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

	return 0;	# if we got to here, then there were no error messages
}








# ---------------------------------------------------------------
# generate_timings_table
# Creates an html table full of all the available program timings
# ---------------------------------------------------------------

sub generate_timings_table {

    my $progs_dir  = "4_examples/progs";


    # table with the test suite summary

    my $summary_text = file_to_string_nofail ("4_examples/progs/logs/summary.txt");
    my $summary = "\n<table border=\"1\" bgcolor=\"#FFEEEE\">\n";
    
    $summary = $summary ."
        <tr>
            <td align=\"center\"><B>Test Suite Settings Summary</B></td>
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
    $table = $table . "<th rowspan=\"2\">Program";
    foreach my $device (@devices)
    {
        $table = $table . "<th colspan=\"2\"><b>$device</b></td>";
    }
    $table = $table . "</tr>\n";


    $table = $table . "<tr>";
    foreach my $device (@devices)
    {
        foreach $projection (@projections)
        {
            my $proj_name = $proj_info{$projection}{"shortname"};
            my $bgcolour  = $proj_info{$projection}{"bgcolour"};

            $table = $table . "<td bgcolor=\"$bgcolour\"><b>$proj_name</b></td>";
        }
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
            foreach $projection (@projections)
            {
                my $time_file = "$progs_dir/logs/$source" ."_mpp_$projection.$device.time";
                my $time_text = file_to_string_nofail($time_file);
                my $hTimes    = parse_timing_text($time_text);
                my $bgcolour  = $proj_info{$projection}{"bgcolour"};
                my $time      = "";
                
                if ($hTimes{"ok"} eq "y")
                {
                    $time = $hTimes{"user"};
                }
                else
                {
                    $time = "<b>" . $hTimes{"string"} . "</b>";
                }

                $table = $table . "<td bgcolor=\"$bgcolour\">$time</td>";
            }
        }

        $table = $table . "</tr>\n";
    }
    
    
    # finish off the table
    
    $table = $table . "</table>";
    
    
    # put it all together
    
    my $page_content = $summary . "\n<p>\n" . $table . "\n<p>\n";
    
    return $page_content;
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

	foreach my $item (@cat_menu)
	{
        	my $title  = $item->{name};
        	my $source = $item->{first};
        
        	$html = $source . "_" . $projection . ".html";
        
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
        my $title  = $item->{source};	
        my $html   = $item->{html};
        my $source = $item->{source};
        
        if ($item->{cat} eq $cat)
        {
            if ($html eq "")
            {
                $html = $source . "_" . $projection . ".html"
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
    
    my $srcfile    = "$progs_dir/src/classic/$source.f";
    my $notesfile  = "$progs_dir/src/classic/$source.txt";
    my $time_mnew  = "$progs_dir/logs/$source" ."_mpp_$projection.PS.time";
    my $time_mold  = "$progs_dir/logs/$source" ."_m69_$projection.time";
    my $vlog_srcn  = "$progs_dir/logs/$source" ."_mpp_$projection.vlog";
    my $exe_mnew   = "$progs_dir/bin/$source" ."_mpp";
    my $notes_table = "";
    my $proj_selection = "";
    my $dev_selection  = "";
    my $srccode  = fortran_file_to_string( $srcfile);
    my $pic_mnew       = $source . "_mpp_$projection" . "_med.gif";
    my $pic_mnew_thumb = $source . "_mpp_$projection" . "_small.gif";
    my $pic_mold       = $source . "_m69_$projection" . "_med.gif";
    my $pic_mold_thumb = $source . "_m69_$projection" . "_small.gif";
    my $pic_montage    = $source . "_both_$projection.gif";
    my $pic_montage_thumb = $source . "_both_small_$projection.gif";
    my $ps_mnew        = $source . "_mpp_$projection.ps";
    my $ps_mold        = $source . "_m69_$projection.ps";
    my $log_mnew       = $source . "_mpp_$projection.PS.log";
    my $log_mold       = $source . "_m69_$projection.log";
    my $err_mnew       = $source . "_mpp_$projection.PS.err";
    my $err_mold       = $source . "_m69_$projection.err";
    my $vlog_mnew      = $source . "_mpp_$projection.vlog";

    my $filesizeold    = filesize ("$psdir/$ps_mold");
    my $filesizenew    = filesize ("$psdir/$ps_mnew");
    
    
        
    # if this example does not have a MAGICS6.9 output, then
    # put in dummy images instead

    if ($no69 eq "y")
    {
        $pic_mold       = "no69.gif";
        $pic_mold_thumb = "no69_small.gif";
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


    my $program_details  = program_listing ($exe_mnew);
    my $lastmod_details  = last_modified_details ("$exe_mnew");
    my $lastps_details   = last_modified_details ("$psdir/$ps_mnew");
#   my $lastvlog_details = last_modified_details ("$vlog_srcn");


    my $time_mnew_text = file_to_string_nofail($time_mnew);
    my $time_mold_text = file_to_string_nofail($time_mold);
	
    my $errors_in_old_magics = check_for_error_messages("$progs_dir/logs/$log_mold");
    my $errors_in_new_magics = !check_mpp_for_completion_message("$progs_dir/logs/$log_mnew");

    my $test_suite_selection = test_suite_selection_html("Classic");



    # our table that allows the user to select between the different
    # projections for this example. Just defining the HTML at the moment;
    # we'll add it to the rest a little later.

#    $proj_selection = "
#    <P>
#    <TABLE border = \"1\" bgcolor = \"#FFEEEE\">
#    <TR>
#        <TD align=\"center\"><B>Projections</B></TD>
#    </TR>
#    <TR>
#        <TD>";


    $proj_selection = "
    <P>
    <TABLE border = \"1\" bgcolor = \"#FFEEEE\">
    <TR>
        <TD align=\"center\"><B>Projections: </B>";
     
    foreach my $proj (@projections)
    {
        my $lowproj = ucfirst (lc($proj));
        if ($proj eq $projection)
        {
            $proj_selection = $proj_selection . $lowproj;
        }
        
        else
        {
            my $link = "<A HREF=\"$source" . "_" . "$proj.html\">";
            $proj_selection = $proj_selection . $link . "$lowproj</A>";
        }
        
        $proj_selection = $proj_selection . ", ";
    }

    $proj_selection = $proj_selection . "
    </TD>
    </TR>
    </TABLE>
    <P>
    ";


    # our portion of the table that gives the user access to the different output formats.
    # Just defining the HTML at the moment; we'll add it to the rest a little later.


    foreach my $device (@devices)
    {
        # do not do for PostScript (this is handled elsewhere) or for SVG (if disabled for this plot)

        if ($device ne 'PS')  
        {
            my $lowdev   = lc($device);
            my $outfile  = "$source" . "_mpp_$projection.$lowdev";
            my $filesize = filesize ("$psdir/$outfile");
            my $uselink  = 0;
            my $name     = $device;
            my $link     = "<A HREF=\"$outfile\">";


            # the first (and maybe only) page of output

            $dev_selection = $dev_selection . "
            <TR>
                <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"$device\">" . $link . " $name</A> ($filesize)";


            # each subsequent page, if they exist

            for (my $page = 1; $page < $max_pages; ++$page)
            {
                $uselink = 0;
                $name    = $device;
                
                $outfile  = "$source" . "_mpp_$projection" . "_0" . "$page.$lowdev";

                if (-e "$psdir/$outfile")
                {
                    $filesize = filesize ("$psdir/$outfile");
                    $name     = "Page " . ($page + 1) . "($filesize)";
                    $link     = "<A HREF=\"$outfile\">";
                    
                    $dev_selection = $dev_selection . ", $link" . " $name</A>";
                }
            }
            

            # finish up for this device

            $dev_selection = $dev_selection . "
                     </TD>
                <TD> </TD>
                <TD> </TD>
            </TR>
            ";

        }
    }





#    foreach my $device (@devices)
#    {
#        my $lowdev   = lc($device);
#        my $outfile  = "$source" . "_mpp_$projection.$lowdev";
#        my $filesize = filesize ("$psdir/$outfile");
#
#        if ($device ne 'PS')
#        {
#            my $link = "<A HREF=\"$outfile\">";
#
#            $dev_selection = $dev_selection . "
#            <TR>
#                <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"$device\">" . $link . " $device</A> ($filesize)</TD>
#                <TD> </TD>
#                <TD> </TD>
#            </TR>
#            ";
#        }
#    }



	# create another row of the table for the case where the old MAGICS
	# has had error messages

	my $errs_header = "";
	my $errs_old    = "";
	my $errs_new    = "";

	if ($errors_in_old_magics || $errors_in_new_magics)
	{
		if ($errors_in_old_magics)
		{
			$errs_old = "<TD  align=\"center\" bgcolor=\"#FF5555\"> ERRORS reported by MAGICS 6</TD>";
		}
		else
		{
			$errs_old = "<TD></TD>";
		}


		if ($errors_in_new_magics)
		{
			$errs_new = "<TD  align=\"center\" bgcolor=\"#FF5555\"> Magics++ did not complete</TD>";
		}
		else
		{
			$errs_new = "<TD></TD>";
		}
			
			
		$errs_header = "<TR>" . $errs_new . "<TD> </TD>" . $errs_old . "</TR>"
	}
	
	


    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . "<P><P>"
               . "<tbody>" 
               . "  <tr>" 
                . "    <td  class=\"menuheading\">$desc</td>"
               . "  </tr>" 
               . "  <tr>" 
                . "    <td   bgcolor=\"#ffffff\">
    <P><P>
    <P>
    <CENTER>
    $test_suite_selection
    <P>
    <TABLE border = \"1\" bgcolor = \"#EEEEFF\">
    <TR>
        <TD> Magics++ executable generated: </TD>
        <TD> <B>$lastmod_details</B> </TD>
        <TD> </TD>
    </TR>
    <TR>
        <TD> Magics++ postscript generated: </TD>
        <TD> <B>$lastps_details</B> </TD>
        <TD> </TD>
    </TR>
    </TABLE>
    $proj_selection
    <TABLE>
	$errs_header
    <TR>
        <TD> <A HREF=\"$ps_mnew\"><IMG SRC=\"$pic_mnew_thumb\" ALT=\"MAGICS++ Plot\"></A> </TD>
        <TD> <A HREF=\"$pic_montage\"><IMG SRC=\"$pic_montage_thumb\" ALT=\"Montage\"></A> </TD>
        <TD> <A HREF=\"$ps_mold\"><IMG SRC=\"$pic_mold_thumb\" ALT=\"MAGICS 6.9 Plot\"></A> </TD>
    </TR>
    <TR>
        <TD><I>Magics++ output - click to enlarge</I></TD>
        <TD><I>Montage</I></TD>
        <TD><I>Magics 6.9 output - click to enlarge</I></TD>
    </TR>
    <TR>
        <TD> Time: $time_mnew_text</TD>
        <TD> </TD>
        <TD> Time: $time_mold_text</TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"MAGICS++ PostScript\">   <A HREF=\"$ps_mnew\">Postscript</A> ($filesizenew)</TD>
        <TD> </TD>
        <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"MAGICS 6.9 PostScript\"> <A HREF=\"$ps_mold\">Postscript</A> ($filesizeold)</TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <A HREF=\"$log_mnew\">Log file</A> </TD>
        <TD> </TD>
        <TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS 6.9 Log\"> <A HREF=\"$log_mold\">Log file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"err_icon.gif\" ALT=\"MAGICS++ Error\"> <A HREF=\"$err_mnew\">Error file</A> </TD>
        <TD> </TD>
        <TD> <IMG SRC=\"err_icon.gif\" ALT=\"MAGICS 6.9 Error\"> <A HREF=\"$err_mold\">Error file</A> </TD>
    </TR>
    $dev_selection
    </TABLE>
    $notes_table
    </CENTER>
    <HR>
    <P>
    <TABLE border = \"1\" bgcolor = \"#FFFFEE\">
    <TR>
        <TD align=\"center\"><B>Executable Listing</B></TD>
    </TR>
    <TR>
        <TD>
            $program_details
            <BR>
        </TD>
    </TR>
    <TR>
        <TD align=\"center\"><B>Magics++ library directory (from \$MAGPLUSLIB)</B></TD>
    </TR>
    <TR>
        <TD>
            $magplusdir
        </TD>
    </TR>
    </TABLE>

    <P>
	<pre class=\"Source\">
     $srccode
    </pre>
						</td>"
               . "  <tr>" 
			   . " <td   bgcolor=\"#ffffff\"> &nbsp; </td>"
               . "  </tr>" 
               . "  </tr>";
                   
#     $default = $default .   "  </tr>";
               


    $default = $default . "
    <TABLE border = \"1\" bgcolor = \"#EEEEFF\">
    <TR>
        <TD align=\"center\"><B>Viewing the Plots</B></TD>
    </TR>
    <TR>
        <TD>
        One easy way to compare the outputs from MAGICS 6.9 and Magics++ is
        to open each GIF plot in a separate tab in Mozilla by clicking each plot with
        the <B>middle</B> mouse button. You can then flick between the tabs to compare.
        Also provided are a montage of the plots as a single image and the original
        postscript files.
        </TD>
    </TR>
    </TABLE>
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







# -------------------------------------------------------
# Main loop - for each projection, generate the html
# -------------------------------------------------------

foreach $projection (@projections)
{

    # -------------------------------------------------------
    # Inner loop - for each example program, generate the html
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

        $html = "html/classic/fortran/$source" . "_" . "$projection.html";
        $/ = undef;
        open(IN,file_to_string("common/template.htmlx"));
        my $text = <IN>;
        close(IN);
        $text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

        open HTML, ">" . $html;
        print HTML $text;
        close HTML;
    }
}







