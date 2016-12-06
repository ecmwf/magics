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
my @menu        = example_menu   (); 
my @cat_menu    = testsuite_submenus (); 
my @projections = projections    ();
my @devices     = ($testprogs_type eq "drivers") ? all_devices () : devices ();
my %devices_info = devices_info ();
my %proj_info   = projections_info ();
my $max_pages   = 15;
my $page_title  = "Magics++ Test Suite";

my $magplusdir  = magics_lib ();
my $psdir       = "html/test/$fortrandir";
my $progs_dir   = "4_examples/progs";


my $small_start = "<font size=\"-2\">";
my $small_end   = "</font>";


# ---------------------------------------------
# magics_lib
# Returns a listing of the MAGPLUSLIB directory
# ---------------------------------------------

sub magics_lib {

    local $magpluslibenv = $ENV{$mpplibname}  || return ("Could not retrieve $mpplibname.");
    local $magpluslib;
    local @alibs;
    my    $strResult = "";



    # split into individual words and store in array

    @alibs = split (/ /, $magpluslibenv);


    # we want the first string

    $magpluslib = @alibs [0];


    # but if shared, then we want to remove the '-L' from the front 
    # not now (20/08/2008)
    #if (substr ($magpluslib, 0, 1) eq '-')
    #{
    #    $magpluslib = substr ($magpluslib, 2, length($magpluslib) - 2);
    #}


    $strResult = $strResult . $magpluslibenv . "\n";


    # get a directory listing

    $strResult = $strResult . "<pre><font size=\"-2\">" . `ls -lh $magpluslib`;

    $strResult = $strResult . "</font></pre>";

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


    # table with the test suite summary

    my $summary_text = file_to_string_nofail ("4_examples/progs/logs/summary.txt");
    my $summary = "\n<table border=\"1\" bgcolor=\"#FFEEEE\">\n";
    
    $summary = $summary ."
        <tr>
            <td align=\"center\"><b>Test Suite Settings Summary</b></td>
        </tr>
        <tr>
            <td>
                <pre><font size=\"-2\">$summary_text</font></pre>
            </td>
        </tr>
        </table>
    ";


	
    # link to the timings files archive

    my $link_to_archive = "\n<table border=\"1\" bgcolor=\"#DDDDFF\">\n
        <tr>
            <td align=\"center\"><a href=\"timings_archive/\">Timings Archive</a></td>
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
    
    my $page_content = $link_to_archive . $summary . "\n<p>\n" . $table . "\n<p>\n";
    
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
	$default = $default . file_to_string("common/location-test.htmlx");
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
        my $item_title  = $item->{name};
        my $item_source = $item->{first};
        
        $html = $item_source . "_" . $projection . ".html";
        
        if ($item_title eq $cat)
        {
            # no link if this is the current category
            $default = $default . "         <tr> <td><b>$item_title</b></td> </tr>\n";
        }
        else
        {
            $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$html\"><b>$item_title</b></a></td> </tr>\n";
        }
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
	    my $item_title  = $item->{name};	
	    my $item_html   = $item->{html};
        my $item_source = $item->{source};
        
        if ($item->{cat} eq $cat)
        {
            if ($item_html eq "")
            {
                $item_html = $item_source . "_" . $projection . ".html"
            }

            if ($item_source eq $source)
            {
                # no link if this is the current example
                $default = $default . "         <tr> <td>$item_title</td> </tr>\n";
            }
            else
            {
                $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$item_html\">$item_title</a></td> </tr>\n";
            }
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


    my $srcfile    = "$progs_dir/src/test/$source.f";
    my $notesfile  = "$progs_dir/src/test/$source.txt";
    my $vlog_srcn  = "$progs_dir/logs/$source" ."_mpp_$projection.vlog";
    my $exe_mnew   = "$progs_dir/bin/$source" ."_mpp";
    my $exe_mold   = "$progs_dir/bin/$source" ."_m69";
    my $time_ps    = "$progs_dir/logs/$source" ."_mpp_$projection.PS.time";
    my $time_gif   = "$progs_dir/logs/$source" ."_mpp_$projection.GIF.time"; #WAS .PNG.time
    my $time_mold  = "$progs_dir/logs/$source" ."_m69_$projection.time";
    my $notes_table    = "";
    my $proj_selection = "";
    my $dev_selection  = "";
    my $srccode  = fortran_file_to_string( $srcfile);
    my $pic_mnew_thumb = $source . "_mpp_$projection" . "_small.gif";
    my $pic_mold_thumb = $source . "_m69_$projection" . "_small.gif";
    my $pic_montage    = $source . "_both_$projection.gif";
    my $pic_montage_thumb = $source . "_both_small_$projection.gif";
    my $ps_mnew        = $source . "_mpp_$projection.ps";
    my $ps_mold        = $source . "_m69_$projection.ps";
    my $gif_mnew       = $source . "_mpp_$projection" . ".gif"; # WAS: "_01.gif"; then "_1.png"
    my $pdf_mnew       = $source . "_mpp_$projection.pdf";
    my $log_mnew_ps    = $source . "_mpp_$projection.PS.log";
    my $log_mnew_gif   = $source . "_mpp_$projection.GIF.log"; # WAS "PNG.log"
    my $log_mold       = $source . "_m69_$projection.log";
    my $err_mnew_ps    = $source . "_mpp_$projection.PS.err";
    my $err_mnew_gif   = $source . "_mpp_$projection.GIF.err"; # WAS "PNG.err"
    my $err_mold       = $source . "_m69_$projection.err";
    my $vlog_mnew_ps   = $source . "_mpp_$projection.vlog";
    my $whole_src_file = "$source.F";
    my $compilation_messages  = $source . "_mpp_compile.log";
    my $lastvlog_details_msg = "";


    my $filesizeold    = filesize ("$psdir/$ps_mold");
    my $pssizenew      = filesize ("$psdir/$ps_mnew");
    my $gifsizenew     = filesize ("$psdir/$gif_mnew");
    my $pdfsizenew     = filesize ("$psdir/$pdf_mnew");
    my $exesizeold     = filesize ("$exe_mold");
    my $exesizenew     = filesize ("$exe_mnew");
    
    my $montage_column1 = "<td></td>";
    my $montage_column2 = "<td></td>";
    
    
    
    
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
        <p>
        <table border = \"1\" bgcolor = \"#EEEEEE\">
        <tr>
            <td align=\"center\"><b>Notes</b></td>
        </tr>
        <tr>
            <td>
                $notes
            </td>
        </tr>
        </table>
        <p>
        "
    }


    my $program_details  = program_listing ($exe_mnew);
    my $lastmod_details  = last_modified_details ("$exe_mnew");
    my $lastmodold_details = last_modified_details ("$exe_mold");
    my $lastps_details   = last_modified_details ("$psdir/$ps_mnew");
    my $lastgif_details  = last_modified_details ("$psdir/$source" . "_mpp_$projection" . ".gif");   # WAS "_1.png"
    my $lastvlog_details = last_modified_details ("$vlog_srcn");
    my $lastvlog_msg     = get_valgrid_summary ("$vlog_srcn");
    

    my $time_mold_text = file_to_string_nofail($time_mold);
    my $time_ps_text   = file_to_string_nofail($time_ps);
    my $time_gif_text  = file_to_string_nofail($time_gif);
	
    my $errors_in_old_magics = check_for_error_messages("$progs_dir/logs/$log_mold");
    my $errors_in_new_magics = !check_mpp_for_completion_message("$progs_dir/logs/$log_mnew_ps");

    my $suite_name = ($fortrandir eq "fortran_static") ? "Fortran_Static" : "Fortran";
    my $test_suite_selection = test_suite_selection_html($suite_name);


    # if the Magics++ program did not compile, then create a small table to
    # highlight the fact, givnig a link to the compilation messages
    
    if (-e $exe_mnew)
    {
        $compilation_problems = "";
    }
    else
    {
        $compilation_problems = "
        <table><tr> <td  align=\"center\" bgcolor=\"#FF9999\"> Magics++ did not compile: <a href=\"$compilation_messages\">messages</a></td></tr></table>";
    }



    $proj_selection = "
    <p>
    <table border = \"1\" bgcolor = \"#FFEEEE\">
    <tr>
        <td align=\"center\">$small_start<b>Projections: </b>";
     
    foreach my $proj (@projections)
    {
        my $lowproj = ucfirst (lc($proj));
        if ($proj eq $projection)
        {
            $proj_selection = $proj_selection . $lowproj;
        }
        
        else
        {
            my $link = "<a href=\"$source" . "_" . "$proj.html\">";
            $proj_selection = $proj_selection . $link . "$lowproj</a>";
        }
        
        $proj_selection = $proj_selection . ", ";
    }

    $proj_selection = $proj_selection . " $small_end
    </td>
    </tr>
    </table>
    ";


    # our portion of the table that gives the user access to the different output formats.
    # Just defining the HTML at the moment; we'll add it to the rest a little later.

    my $toprow_info = "";
    my $toprow_img  = "";

    foreach my $device (@devices)
    {
        # do not do for PostScript (this is handled elsewhere) or for SVG (if disabled for this plot)

        if ($device ne 'PS')  
        {
            my $lowdev   = lc($device);
            my $time_mnew  = "$progs_dir/logs/$source" ."_mpp_$projection.$device.time";
            my $name     = $device;
            my $time_mnew_text = file_to_string_nofail($time_mnew); # load up the time logs for this device
            my $dev_info = %devices_info->{$device};

            # jpeg -> .jpg
            
            if ($lowdev eq "jpeg") {$lowdev = "jpg";}


#            print ("XXXXXXXXXXXX\n");
#            print (%devices_info);
#            print ("TRY: $device, $dev_info->{multifile}\n");


            my $info;
            my $outfile1;
            my $link1;
            my $embedded_img;

            if ($dev_info->{multifile} eq "y")
            {
                my $page     = 1;
#               my $outfile  = "$source" . "_mpp_$projection.$lowdev";
                my $outfile  = "$source" . "_mpp_$projection" . "_" . "$page.$lowdev";
                   $outfile1 = $outfile;
                my $filesize = filesize ("$psdir/$outfile");
                my $link     = "<a href=\"$outfile\">";
                   $link1    = "<a href=\"$outfile1\">";
#                print ("multi: $outfile\n");


                # the first (and maybe only) page of output

                $info = "<img src=\"ps_icon.gif\" alt=\"$device\">" . $link . " $name</a> ($filesize)";


                # each subsequent page, if they exist

                for ($page = 2; $page < $max_pages; ++$page)
                {
                    $name    = $device;

    #                $outfile  = "$source" . "_mpp_$projection" . "_0" . "$page.$lowdev";
                    $outfile  = "$source" . "_mpp_$projection" . "_" . "$page.$lowdev";

                    if (-e "$psdir/$outfile")
                    {
                        $filesize = filesize ("$psdir/$outfile");
                        $name     = "Page " . ($page) . "($filesize)";
                        $link     = "<a href=\"$outfile\">";

                        $info = $info . ", $link" . " $name</a>";
                    }
                }

                $info = $info . "<br>[Time: $time_mnew_text]";

                # finish up for this device

                $embedded_img = html_embed($dev_info->{embedtag}, $outfile1, 252);

                if ($dev_info->{toprow} eq "n")  # do not add to table if it's already at the top
                {
                    $dev_selection = $dev_selection . "
                    <tr>
                        <td> $info
                             </td>
                        <td>$link1$embedded_img</a></td>
                        <td></td>
                    </tr>
                    ";
                }
            }
            
            else
            {
                my $outfile  = "$source" . "_mpp_$projection." . $lowdev;
                my $filesize = filesize ("$psdir/$outfile");
                my $link     = "<a href=\"$outfile\">";
                   $link1    = $link;
#                print ("single: $outfile\n");

                $info = "<img src=\"ps_icon.gif\" alt=\"$device\">" . $link . " $name</a> ($filesize)";


                $info = $info . "<br>[Time: $time_mnew_text]";

                # finish up for this device

                $embedded_img = html_embed($dev_info->{embedtag}, $outfile, 252);

                if ($dev_info->{toprow} eq "n")  # do not add to table if it's already at the top
                {
                    $dev_selection = $dev_selection . "
                    <tr>
                        <td> $info
                             </td>
                        <td>$link$embedded_img</a></td>
                        <td></td>
                    </tr>
                    ";
                }
            }

            if ($dev_info->{toprow} eq "y")
            {
                $toprow_info = $info;
                $toprow_img  = "$link1$embedded_img</a>";
            }
        }
    }


	# create another row of the table for the case where the old MAGICS
	# has had error messages

	my $errs_header = "";
	my $errs_old    = "";
	my $errs_new    = "";

	if ($errors_in_old_magics || $errors_in_new_magics)
	{
		if ($errors_in_old_magics)
		{
			$errs_old = "<td  align=\"center\" bgcolor=\"#FF5555\"> ERRORS reported by MAGICS 6</td>";
		}
		else
		{
			$errs_old = "<td></td>";
		}


		if ($errors_in_new_magics)
		{
			$errs_new = "<td  align=\"center\" bgcolor=\"#FF5555\"> Magics++ did not complete</td>";
		}
		else
		{
			$errs_new = "<td></td>";
		}
			
			
		$errs_header = "<tr>" . $errs_new . "<td> </td>" . $errs_old . "</tr>"
	}
	



	# check to see whether the montage is actually up-to-date (or even exists)
	# if not, then do not have the montage column

    if (-e "$psdir/$pic_montage_thumb")
    {
		if ((stat("$psdir/$pic_montage_thumb"))[9] > (stat("$psdir/$ps_mnew"))[9])
        {
                $montage_column1 = "<td> <a href=\"$pic_montage\"><img src=\"$pic_montage_thumb\" ALT=\"Montage\"></A> </td>";
                $montage_column2 = "<td><I>Montage</I></td>";
        }
        else
        {
                $montage_column1 = "<td></td>";
                $montage_column2 = "<td></td>";
        }
    }
	else
	{
            # no montage column - let's put in a thumbnail of and link to the GIF image
	    
            my $giffile  = "$source" . "_mpp_$projection" . ".gif"; # WAS _1.png
            my $gifthumb = "$source" . "_mpp_$projection" . ".gif"; #"_gif_small.gif";
            $montage_column1 = "<td>$toprow_img</td>";
            $montage_column2 = "<td><i>Magics++ output</i></td>";
	}



	
    if ($lastvlog_details eq "File not found")
    {
        $lastvlog_details_msg = $lastvlog_details;
    }
    else
    {
        $lastvlog_details_msg = "generated: $lastvlog_details, $lastvlog_msg";
    }

    my $mag_version = magplus_version_string();


    $default = "<table background=\"/assets/shim.gif\" bgcolor=\"#FFF1B3\" border=\"0\""
               . "cellpadding=\"1\" cellspacing=\"0\" width=\"100%\" class=\"submenu\">"
               . ""
               . "<tbody>" 
               . "  <tr>" 
                . "    <td class=\"menuheading\">[<b>$mag_version</b>] $desc</td>"
               . "  </tr>" 
               . "  <tr>" 
                . "    <td bgcolor=\"#ffffff\">
    <center>
    $test_suite_selection
    $proj_selection
    <p>
    <table border = \"1\" bgcolor = \"#EEEEFF\">
    <tr>
        <td>$small_start Magics++ executable generated:$small_end </td>
        <td> $small_start<B>$lastmod_details, $exesizenew</B>$small_end </td>
        <td> </td>
    </tr>
    <tr>
        <td> ${small_start}Magics++ postscript generated:$small_end </td>
        <td> $small_start<B>$lastps_details</B>$small_end </td>
        <td> </td>
    </tr>
    <tr>
        <td> ${small_start}Magics++ GIF generated:$small_end </td>
        <td> $small_start<B>$lastgif_details</B>$small_end </td>
        <td> </td>
    </tr>
    <tr>
        <td> ${small_start}Magics 6 executable generated:$small_end </td>
        <td> $small_start<B>$lastmodold_details, $exesizeold</B>$small_end </td>
        <td> </td>
    </tr>
    </table>
    $compilation_problems
    <table>
    <colgroup>
      <col width=\"30%\">
    </colgroup>
    $errs_header
    <tr>
        <td> <a href=\"$ps_mnew\"><img src=\"$pic_mnew_thumb\" alt=\"MAGICS++ Plot\"></a> </td>
        $montage_column1
        <td> <a href=\"$ps_mold\"><img src=\"$pic_mold_thumb\" alt=\"MAGICS 6.9 Plot\"></a> </td>
    </tr>
    <tr>
        <td><i>Magics++ output</i></td>
        $montage_column2
        <td><i>Magics 6.10 output</i></td>
    </tr>
    <tr>
        <td> <img src=\"ps_icon.gif\" ALT=\"MAGICS++ PostScript\">   <a href=\"$ps_mnew\">Postscript</A> ($pssizenew),  <a href=\"$pdf_mnew\">PDF</A> ($pdfsizenew)<br>[Time: $time_ps_text]<br></td>
        <td> $toprow_info</td>
        <td> <img src=\"ps_icon.gif\" ALT=\"MAGICS 6.9 PostScript\"> <a href=\"$ps_mold\">Postscript</A> ($filesizeold) <br>[Time: $time_mold_text]</td>
    </tr>
    <tr>
        <td> <img src=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <a href=\"$log_mnew_ps\">Log file</A> </td>
        <td> </td>
        <td> <img src=\"log_icon.gif\" ALT=\"MAGICS 6.9 Log\"> <a href=\"$log_mold\">Log file</A> </td>
    </tr>
    <tr>
        <td> <img src=\"err_icon.gif\" ALT=\"MAGICS++ Error\"> <a href=\"$err_mnew_ps\">Error file</A> </td>
        <td> </td>
        <td> <img src=\"err_icon.gif\" ALT=\"MAGICS 6.9 Error\"> <a href=\"$err_mold\">Error file</A> </td>
    </tr>
    <tr>
        <td> <img src=\"valgrind.gif\" ALT=\"MAGICS++ Valgrind Output\"> <a href=\"$vlog_mnew_ps\">Valgrind output</A>
		     ($lastvlog_details_msg)</td>
        <td> </td>
        <td> </td>
    </tr>
    $dev_selection
    </table>
    $notes_table
    </center>
    <hr>
    <p>
    <table border = \"1\" bgcolor = \"#FFFFEE\">
    <tr>
        <td align=\"center\" bgcolor = \"#EEEEDD\"><b>Download Source</b>: <a href=\"$whole_src_file\">$whole_src_file</a></td>
    </tr>
    <tr>
        <td align=\"center\"><B>Executable Listing</B></td>
    </tr>
    <tr>
        <td>
            $program_details
            <br>
        </td>
    </tr>
    <tr>
        <td align=\"center\"><B>Magics++ library directory (from \$$mpplibname)</B></td>
    </tr>
    <tr>
        <td>
            $magplusdir
        </td>
    </tr>
    </table>

    <p>
	<pre class=\"Source\"><font size=\"-2\">
     $srccode
    </font></pre>
						</td>"
               . "  <tr>" 
			   . " <td   bgcolor=\"#ffffff\"> &nbsp; </td>"
               . "  </tr>" 
               . "  </tr>";
                   
#     $default = $default .   "  </tr>";
               


    $default = $default . "
    <table border = \"1\" bgcolor = \"#EEEEFF\">
    <tr>
        <td align=\"center\"><B>Viewing the Plots</B></td>
    </tr>
    <tr>
        <td>
        One easy way to compare the outputs from MAGICS 6.9 and Magics++ is
        to open each PNG plot in a separate tab in Mozilla by clicking each plot with
        the <B>middle</B> mouse button. You can then flick between the tabs to compare.
        Also provided are a montage of the plots as a single image and the original
        postscript files.
        </td>
    </tr>
    </table>
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

$timings = 0;  # no, we're not generating the timings page yet

foreach $projection (@projections)
{

    # -------------------------------------------------------
    # Inner loop - for each example program, generate the html
    # -------------------------------------------------------

    foreach my $example (@menu)
    {
        my $title   = $example->{name};	
        my $html    = "";
           $source  = $example->{source};
           $desc    = $example->{description};
           $no69    = $example->{no69};
        my $lang    = $example->{lang};
           $cat     = $example->{cat};
        my $drivers = $example->{drivers};


        # if running the 'drivers' test suite, then only generate the relevant pages

        if (($testprogs_type eq "drivers") and ($drivers ne "y"))
        {
            next;
        }


        if ($lang eq "fortran")
        {
            $html = "html/test/$fortrandir/$source" . "_" . "$projection.html";
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
}



# ------------------------------------------------------------
# Now we want to generate a page that contains all the timings
# ------------------------------------------------------------

$timings = 1;  # we are generating the timings page
$projection = @projections[0];

$timingshtml = "html/test/$fortrandir/timings.html";

$/ = undef;
open(IN,file_to_string("common/template.htmlx"));
my $text = <IN>;
close(IN);
$text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

open HTML, ">" . $timingshtml;
print HTML $text;
close HTML;


