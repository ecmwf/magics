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

my $ecmwf       = file_to_string ("common/ecmwf.htmlx");
my @menu        = c_testsuite_menu (); 
my @cat_menu    = c_testsuite_submenus (); 
my @devices     = c_devices ();
my @projections = projections    ();
my %proj_info   = projections_info ();
my $max_pages   = 15;
my $lib_version = 'MAGPLUSLIB_SHARED';
my $testsuite   = ($testprogs_type eq "test") ? 1 : 0;
my $page_title  = ($testsuite) ? "Magics++ C Test Suite" : "Magics++ C Examples";



# ---------------------------------------------
# magics_lib
# Returns a listing of the MAGPLUSLIB directory
# ---------------------------------------------

sub magics_lib {

    local $magpluslibenv = $ENV{'MAGPLUSLIB_STATIC'}  || return ("Could not retrieve $mpplibname.");
    local $magpluslib;
    local @alibs;
    my    $strResult = "<PRE>";


    # split into individual words and store in array

    @alibs = split (/ /, $magpluslibenv);


    # we want the first string

    $magpluslib = @alibs [0];

    # $magpluslib = substr ($magpluslib, 2, length($magpluslib) - 2);


    # get a directory listing

    $strResult = $strResult . `ls -lh $magpluslib`;

    $strResult = $strResult . "</PRE>";

    return $strResult;
}





# ------------------------------------------------------------------------
# link_to_FORTRAN_example
# If there is a FORTRAN example of the same name, then create a link to it
# ------------------------------------------------------------------------

sub link_to_FORTRAN_example {

    local ($basename)       = $_[0];
    local ($pathtofortran)  = $_[1];
    local ($projection)     = $_[2];
    
    
    $hFORTRAN{"exists"} = "n";

    my $strFortranName = "$pathtofortran/$basename.f";

    if (-e $strFortranName)   # does the FORTRAN example exist?
    {
        $hFORTRAN{"exists"}     = "y";
        $hFORTRAN{"image_link"} = "<img src=\"../test/${basename}_mpp_${projection}_small.gif\" ALT=\"FORTRAN Plot\">";
        $hFORTRAN{"html_link"}  = "<a href=\"../test/${basename}_${projection}.html\"> Link to FORTRAN Page</a>";
        $hFORTRAN{"ps_link"}    = "<a href=\"../test/${basename}_mpp_${projection}.ps\"> FORTRAN Postscript</a>";
    }

    return $hFORTRAN;
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
    $default = $default . "<a href=\"index.html\">C</a>";
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

    my $flat = ($testsuite) ? 0 : 1;  # flat means no hierarchical menus of examples


    if (!$flat)
    {
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


        # add links to each category

        foreach my $item (@cat_menu)
        {
            my $title  = $item->{name};
            my $source = $item->{first};

            $html = $source . $projection_ext . ".html";

            $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$html\"><b>$title</b></a></td> </tr>\n";
        }


        $default = $default . "</table>"
                    . "        </table>"
                    . "    </td>"
                    . "   </tr>"
                    . "   </table>";

    }


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
        my $title   = $item->{name};    
        my $html    = $item->{html};
        my $source  = $item->{source};
        my $fordocs = $item->{fordocs};

        if ($flat || $item->{cat} eq $cat)
        {
            if ($testsuite || ($fordocs eq "yes"))
            {
                if ($html eq "")
                {
                    $html = $source . $projection_ext . ".html"
                }

                $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$html\">$title</a></td> </tr>\n";
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

    my $srcsubdir  = ($testsuite) ? "test" : "www";
    my $htmldir    = ($testsuite) ? "html/test/c" : "html/examples/c";
    my $psdir      = $htmldir;
    my $progs_dir  = "4_examples/progs";
    my $srcfile    = "$progs_dir/c/$srcsubdir/$source.c";
    my $notesfile  = "$progs_dir/c/$srcsubdir/$source.txt";
    my $vlog_srcn  = "$progs_dir/logs/$source" ."_mpp$projection_ext.vlog";
    my $exe_mnew   = "$progs_dir/bin/$source" ."_mpp";
    my $time_ps    = "$progs_dir/logs/$source" ."_mpp$projection_ext.PS.time";
    my $time_gif   = "$progs_dir/logs/$source" ."_mpp$projection_ext.GIF.time";
    my $time_mold  = "$progs_dir/logs/$source" ."_m69$projection_ext.time";
    my $notes_table    = "";
    my $proj_selection = "";
    my $dev_selection  = "";
    my $srccode  = c_file_to_string( $srcfile);
    my $pic_mnew       = $source . "_mpp$projection_ext" . "_med.gif";
    my $pic_mnew_thumb = $source . "_mpp$projection_ext" . "_small.gif";
    my $pic_mold       = $source . "_m69$projection_ext" . "_med.gif";
    my $pic_mold_thumb = $source . "_m69$projection_ext" . "_small.gif";
    my $pic_montage    = $source . "_both$projection_ext.gif";
    my $pic_montage_thumb = $source . "_both_small$projection_ext.gif";
    my $ps_mnew        = $source . "_mpp$projection_ext.ps";
    my $ps_mold        = $source . "_m69$projection_ext.ps";
    my $gif_mnew       = $source . "_mpp$projection_ext" . "1.gif"; # WAS: "_01.gif";
    my $pdf_mnew       = $source . "_mpp$projection_ext.pdf";
    my $log_mnew_ps    = $source . "_mpp$projection_ext.PS.log";
    my $log_mnew_gif   = $source . "_mpp$projection_ext.GIF.log";
    my $log_mold       = $source . "_m69$projection_ext.log";
    my $err_mnew_ps    = $source . "_mpp$projection_ext.PS.err";
    my $err_mnew_gif   = $source . "_mpp$projection_ext.GIF.err";
    my $err_mold       = $source . "_m69$projection_ext.err";
    my $vlog_mnew_ps   = $source . "_mpp$projection_ext.vlog";

    my $filesizeold    = filesize ("$psdir/$ps_mold");
    my $pssizenew      = filesize ("$psdir/$ps_mnew");
    my $gifsizenew     = filesize ("$psdir/$gif_mnew");
    my $pdfsizenew     = filesize ("$psdir/$pdf_mnew");
    
    my $montage_column1 = "<TD></TD>";
    my $montage_column2 = "<TD></TD>";

    my $test_suite_selection = ($testsuite) ? test_suite_selection_html("C") : "";

  

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




#    my $magplusdir       = magics_lib ();
#    my $program_details  = program_listing ($exe_mnew);
#    my $lastmod_details  = last_modified_details ("$exe_mnew");
#    my $lastps_details   = last_modified_details ("$psdir/$ps_mnew");
#    my $lastgif_details  = last_modified_details ("$psdir/$source" . "_mpp$projection_ext" . "1.gif");
#    my $lastvlog_details = last_modified_details ("$vlog_srcn");
#
#    my $time_mold_text = file_to_string_nofail($time_mold);
#    my $time_ps_text   = file_to_string_nofail($time_ps);
#    my $time_gif_text   = file_to_string_nofail($time_gif);
#	
#    my $errors_in_old_magics = check_for_error_messages("$progs_dir/logs/$log_mold");
#    my $errors_in_new_magics = !check_mpp_for_completion_message("$progs_dir/logs/$log_mnew_ps");


#    my $test_suite_selection = test_suite_selection_html("C");



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


    if ($testsuite)
    {
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
        ";
    }


    # our portion of the table that gives the user access to the different output formats.
    # Just defining the HTML at the moment; we'll add it to the rest a little later.


    if ($testsuite)
    {
        foreach my $device (@devices)
        {
            # do not do for PostScript (this is handled elsewhere) or for SVG (if disabled for this plot)

            if ($device ne 'PS')  
            {
                my $lowdev   = lc($device);
                my $page     = 1;
    #           my $outfile  = "$source" . "_mpp$projection_ext.$lowdev";
                my $outfile  = "$source" . "_mpp$projection_ext" . "_0" . "$page.$lowdev";
                my $time_mnew  = "$progs_dir/logs/$source" ."_mpp$projection_ext.$device.time";
                my $filesize = filesize ("$psdir/$outfile");
                my $uselink  = 0;
                my $name     = $device;
                my $link     = "<A HREF=\"$outfile\">";


                # load up the time logs for this device

                my $time_mnew_text = file_to_string_nofail($time_mnew);


                # the first (and maybe only) page of output

                $dev_selection = $dev_selection . "
                <TR>
                    <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"$device\">" . $link . " $name</A> ($filesize)";


                # each subsequent page, if they exist

                for ($page = 2; $page < $max_pages; ++$page)
                {
                    $uselink = 0;
                    $name    = $device;

                    $outfile  = "$source" . "_mpp$projection_ext" . "_0" . "$page.$lowdev";

                    if (-e "$psdir/$outfile")
                    {
                        $filesize = filesize ("$psdir/$outfile");
                        $name     = "Page " . ($page + 1) . "($filesize)";
                        $link     = "<A HREF=\"$outfile\">";

                        $dev_selection = $dev_selection . ", $link" . " $name</A>";
                    }
                }

                $dev_selection = $dev_selection . "<BR>[Time: $time_mnew_text]";

                # finish up for this device

                $dev_selection = $dev_selection . "
                         </TD>
                    <TD> </TD>
                    <TD> </TD>
                </TR>
                ";

            }
        }
    }
    
    else
    {
        $dev_selection = "";
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
	



	# check to see whether the montage is actually up-to-date (or even exists)
	# if not, then do not have the montage column

    if (-e "$psdir/$pic_montage_thumb")
    {
		if ((stat("$psdir/$pic_montage_thumb"))[9] > (stat("$psdir/$ps_mnew"))[9])
        {
                $montage_column1 = "<TD> <A HREF=\"$pic_montage\"><IMG SRC=\"$pic_montage_thumb\" ALT=\"Montage\"></A> </TD>";
                $montage_column2 = "<TD><I>Montage</I></TD>";
        }
        else
        {
                $montage_column1 = "<TD></TD>";
                $montage_column2 = "<TD></TD>";
        }
    }
	else
	{
            # no montage column - let's put in a thumbnail of and link to the GIF image
	    
            my $giffile  = "$source" . "_mpp$projection_ext" . "1.gif";
            $montage_column1 = ($testsuite) ? "<TD><A HREF=\"$giffile\"> <IMG width=\"252\" height=\"180\" src=\"$giffile\" ALT=\"MAGICS++ Plot (GIF)\"</A></TD>"  :  "";
            $montage_column2 = ($testsuite) ? "<TD><i>Magics++ (GIF) - click to enlarge</i></TD>"  :  "";
	}






    if ($testsuite)
    {
        $magplusdir         = magics_lib ();
        $lastps_details     = last_modified_details   ("$htmldir/$ps_mnew");
        $lastvlog_details   = last_modified_details   ("$vlog_srcn");
        $time_ps_text       = file_to_string_nofail   ($time_ps);
        my $hFORTRAN           = link_to_FORTRAN_example ($source, "$progs_dir/src/test", $magml_projection);
        my $FORTRAN_html_link  = ($hFORTRAN{"exists"} eq "y") ? $hFORTRAN{"html_link"}  : "No FORTRAN equivalent";
        my $FORTRAN_image_link = ($hFORTRAN{"exists"} eq "y") ? $hFORTRAN{"image_link"} : "No FORTRAN equivalent";
        my $FORTRAN_ps_link    = ($hFORTRAN{"exists"} eq "y") ? $hFORTRAN{"ps_link"}    : "No FORTRAN equivalent";
        
        $table_FORTRAN_image_cell = "<TD bgcolor=\"#EEEEEE\"> $FORTRAN_image_link </TD>";
        $table_FORTRAN_link_cell  = "<TD bgcolor=\"#EEEEEE\"><I>$FORTRAN_html_link</I></TD>";
        $table_FORTRAN_ps_cell    = "<TD bgcolor=\"#EEEEEE\"> <IMG SRC=\"ps_icon.gif\" ALT=\"FORTRAN PostScript\"> $FORTRAN_ps_link </TD>";

        $table_log_rows = 
   "<TR>
        <TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <A HREF=\"$log_mnew\">Log file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"err_icon.gif\" ALT=\"MAGICS++ Error\"> <A HREF=\"$err_mnew\">Error file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"valgrind.gif\" ALT=\"MAGICS++ Valgrind Output\"> <A HREF=\"$vlog_mnew\">Valgrind output</A>
             (generated: $lastvlog_details)</TD>
    </TR>";
    
    
        $ps_extra_text = " ($filesizenew),  <A HREF=\"$pdf_mnew\">PDF</A> ($pdfsizenew)<BR>[Time: $time_ps_text]<BR>('PS_PDF' is 'BOTH')";

    }
    
    else
    {
        $table_FORTRAN_image_cell = "";
        $table_FORTRAN_link_cell  = "";
        $table_FORTRAN_ps_cell    = "";
        $table_log_rows           = "";
        $ps_extra_text            = "";
    }


 
    if ($testsuite)
    {
        $generation_times_table = 
        "
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
    <TR>
        <TD> Magics++ GIF generated: </TD>
        <TD> <B>$lastgif_details</B> </TD>
        <TD> </TD>
    </TR>
    </TABLE>
        ";


    $executable_listing_table =
    "
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
        <TD align=\"center\"><B>Magics++ library directory (from \$$mpplibname)</B></TD>
    </TR>
    <TR>
        <TD>
            $magplusdir
        </TD>
    </TR>
    </TABLE>
    ";


   $gif_plot_col = "<TD> <A HREF=\"$ps_mold\"><IMG SRC=\"$pic_mold_thumb\" ALT=\"MAGICS 6.9 Plot\"></A> </TD>";

   $gif_size_col = "<TD> <IMG SRC=\"ps_icon.gif\" ALT=\"MAGICS++ GIF\">          <A HREF=\"$gif_mnew\">GIF</A> ($gifsizenew)<BR>[Time: $time_gif_text]</TD>";
   $gif_log_col  = "<TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <A HREF=\"$log_mnew_gif\">Log file</A> </TD>";

   $additional_info_rows = 
    "
    <TR>
        <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"MAGICS++ PostScript\">   <A HREF=\"$ps_mnew\">Postscript</A> ($pssizenew),  <A HREF=\"$pdf_mnew\">PDF</A> ($pdfsizenew)<BR>[Time: $time_ps_text]<BR>('PS_PDF' is 'BOTH')</TD>
        $gif_size_col
    </TR>
    <TR>
        <TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <A HREF=\"$log_mnew_ps\">Log file</A> </TD>
        <TD> <IMG SRC=\"log_icon.gif\" ALT=\"MAGICS++ Log\"> <A HREF=\"$log_mnew_gif\">Log file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"err_icon.gif\" ALT=\"MAGICS++ Error\"> <A HREF=\"$err_mnew_ps\">Error file</A> </TD>
        <TD> <IMG SRC=\"err_icon.gif\" ALT=\"MAGICS++ Error\"> <A HREF=\"$err_mnew_gif\">Error file</A> </TD>
    </TR>
    <TR>
        <TD> <IMG SRC=\"valgrind.gif\" ALT=\"MAGICS++ Valgrind Output\"> <A HREF=\"$vlog_mnew_ps\">Valgrind output</A>
		     (generated: $lastvlog_details)</TD>
        <TD> </TD>
        <TD> </TD>
    </TR>
    ";




    }
    
    else
    {
        $generation_times_table   = "";
        $executable_listing_table = "";
        $additional_info_rows     = "";
        $gif_plot_col             = "";
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
    <CENTER>
    $test_suite_selection
    <P>
    $generation_times_table
    $proj_selection
    <TABLE>
    $errs_header
    <TR>
        <TD> <A HREF=\"$ps_mnew\"><IMG SRC=\"$pic_mnew_thumb\" ALT=\"MAGICS++ Plot\"></A> </TD>
        $montage_column1
        $gif_plot_col
    </TR>
    <TR>
        <TD><I>Magics++ output - click to enlarge</I></TD>
        $montage_column2
    </TR>
    $additional_info_rows 
    $dev_selection
    </TABLE>
    $notes_table
    </CENTER>
    <HR>
    <P>
    $executable_listing_table
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
    $projection_ext = ($testsuite) ? "_$projection" : "";  # do we add the projection to the filenames of html docs?

    # -------------------------------------------------------
    # Main loop - for each example program, generate the html
    # -------------------------------------------------------

    foreach my $example (@menu)
    {
        my $title   = $example->{name};    
        my $html    = "";
           $source  = $example->{source};
           $desc    = $example->{description};
           $no69    = $example->{no69};
        my $lang    = $example->{lang};
        my $fordocs = $example->{fordocs};
           $cat     = $example->{cat};
        my $htmlsubdir = ($testsuite) ? "html/test/c" : "html/examples/c";

        if (($lang eq "c") && (($testsuite) || ($fordocs eq "yes")))
        {
            $html = "$htmlsubdir/$source" . $projection_ext . ".html";
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


