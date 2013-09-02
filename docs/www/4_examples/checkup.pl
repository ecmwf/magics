#!/usr/bin/perl

use common::mpp_utils;
use File::stat;
use Cwd;                # for the function 'getcwd'

my $ecmwf       = file_to_string         ("common/ecmwf.htmlx");
my @menu        = checkup_testsuite_menu (); 
my @platforms   = checkup_platforms      ();
my @epsmenu     = epsgram_checkup_menu  (); 
my $max_pages   = 15;
my $page_title  = "Magics++ Checkup Test Suite";





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
    $default = "<TITLE>" . $page_title . " ($platform)" . "</TITLE>";
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



    # find the current working directory, then split it into its components and
    # use that to reassemble into the corresponding path for each different platform.

    my $dir = getcwd;


    # add links to each platform

    foreach my $plat (@platforms)
    {
        # create a link to the new platform by a search/replace operation on the
        # current directory, replacing the current platform with the new one

        my $this_platdir = $dir;

        $this_platdir =~ s/$platform/$plat/g;

        $plathtml = $this_platdir . "/html/checkup/$plat.html";
        
        $default = $default . "         <tr> <td><a class=\"menuitem\" href=\"$plathtml\"><b>$plat</b></a></td> </tr>\n";
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

    my $projection = "CYLINDRICAL";
    my $output_table;



    # create the table of EPSgram outputs ----------------------------------------

    $output_table = "<!-- Table of EPSgram output thumbnails -->\n";
    $output_table = $output_table . "<table>\n";


    foreach my $shell ("csh", "ksh")
    {
        # create a new table rown with this combination of settings

        my $magversion = magics_version_from_dir ();
        my $outdir     = "../../../../../../$shell/$magversion/shared/odb_no/html/checkup/epsgram/";
        my $outdirloc  = "../../../../$shell/$magversion/shared/odb_no/html/checkup/epsgram/";

        my $ps_mnew    = $outdir    . "eps-reading.ps";
        my $ps_mnewloc = $outdirloc . "eps-reading.ps";
        my $mnew_thumb = $outdir    . "eps-reading_small.gif";
        my $ps_details = last_modified_details ("$ps_mnewloc");
        my $row        = "<tr>";
        my $header_row = "<tr><td>EPSgram ($shell) [<b>$ps_details</b>]:</td></tr>";
        my $footer_row = "<tr><td></td></tr>";
        my $title      = "eps-reading - $ps_details";	
        my $cell       = "<td><a href=\"$ps_mnew\"><image src=\"$mnew_thumb\" alt=\"$title\"></a></td>";

        $row = $row .$cell;

        $row = $row . "</tr>\n";
        $output_table = $output_table . $header_row . $row . $footer_row;
    }
    
    $output_table = $output_table . "</table>";



    # create the table of MagML outputs ----------------------------------------

    $output_table = $output_table . "<!-- Table of MagML output thumbnails -->\n";
    $output_table = $output_table . "<table>\n";


    foreach my $shell ("csh", "ksh")
    {
        # create a new table rown with this combination of settings

        my $magversion = magics_version_from_dir ();
        my $outdir     = "../../../../../../$shell/$magversion/shared/odb_no/html/checkup/magml/";
        my $outdirloc  = "../../../../$shell/$magversion/shared/odb_no/html/checkup/magml/";

        my $ps_mnew    = $outdir    . "contour_simple.ps";
        my $ps_mnewloc = $outdirloc . "contour_simple.ps";
        my $mnew_thumb = $outdir    . "contour_simple_small.gif";
        my $ps_details = last_modified_details ("$ps_mnewloc");
        my $row        = "<tr>";
        my $header_row = "<tr><td>MagML ($shell) [<b>$ps_details</b>]:</td></tr>";
        my $footer_row = "<tr><td></td></tr>";
        my $title      = "Simple Contour - $ps_details";	
        my $cell       = "<td><a href=\"$ps_mnew\"><image src=\"$mnew_thumb\" alt=\"$title\"></a></td>";

        $row = $row .$cell;

        $row = $row . "</tr>\n";
        $output_table = $output_table . $header_row . $row . $footer_row;
    }
    
    $output_table = $output_table . "</table>";



    # create the table of C Interface outputs ----------------------------------------

    $output_table = $output_table . "<!-- Table of C output thumbnails -->\n";
    $output_table = $output_table . "<table>\n";


    foreach my $shell ("csh", "ksh")
    {
        # create a new table rown with this combination of settings

        my $magversion = magics_version_from_dir ();
        my $outdir     = "../../../../../../$shell/$magversion/shared/odb_no/html/checkup/fortran/";
        my $outdirloc  = "../../../../$shell/$magversion/shared/odb_no/html/checkup/fortran/";

        my $ps_mnew    = $outdir    . "cont_colours_mpp_CYLINDRICAL.ps";
        my $ps_mnewloc = $outdirloc . "cont_colours_mpp_CYLINDRICAL.ps";
        my $mnew_thumb = $outdir    . "cont_colours_mpp_CYLINDRICAL_small.gif";
        my $ps_details = last_modified_details ("$ps_mnewloc");
        my $compilation_messages = $outdir . "cont_colours_mpp_compile.log";
        my $err_mnew_ps          = $outdir . "cont_colours_mpp_CYLINDRICAL.PS.err";
        my $row        = "<tr>";
        my $header_row = "<tr><td>C ($shell) [<b>$ps_details</b>]"
	                 . "[<a href=\"$compilation_messages\">Compile log</a>]"
			 . "[<a href=\"$err_mnew_ps\">Run errors</a>]:</td></tr>";
        my $footer_row = "<tr><td></td></tr>";
        my $title      = "Contours plot - $ps_details";
        my $cell       = "<td><a href=\"$ps_mnew\"><image src=\"$mnew_thumb\" alt=\"$title\"></a></td>";

        $row = $row .$cell;

        $row = $row . "</tr>\n";
        $output_table = $output_table . $header_row . $row . $footer_row;
    }
    
    $output_table = $output_table . "</table>";


    # create the table of FORTRAN outputs ----------------------------------------

    $output_table = $output_table . "<!-- Table of FORTRAN output thumbnails -->\n";
    $output_table = $output_table . "<table>\n";


    foreach my $shell ("csh", "ksh")
    {
        foreach my $linktype ("static", "shared")
        {
            foreach my $odb ("odb_no") #, "odb_yes"
            {
                # create a new table rown with this combination of settings

                my $row        = "<tr>";
                my $header_row = "<tr>";
                my $footer_row = "<tr><td></td></tr>";
                my $magversion = magics_version_from_dir ();
                my $outdir     = "../../../../../../$shell/$magversion/$linktype/$odb/html/checkup/fortran/";
                my $outdirloc  = "../../../../$shell/$magversion/$linktype/$odb/html/checkup/fortran/";


                foreach my $example (@menu)
                {
                    my $title      = $example->{name};	
                    my $source     = $example->{source};
                    my $ps_mnew    = $outdir    . $source . "_mpp_$projection.ps";
                    my $ps_mnewloc = $outdirloc . $source . "_mpp_$projection.ps";
                    my $mnew_thumb = $outdir    . $source . "_mpp_$projection" . "_small.gif";
                    my $ps_details = last_modified_details ("$ps_mnewloc");
                    my $compilation_messages = $outdir . $source . "_mpp_compile.log";
                    my $runout_messages = $outdir . $source . "_CYLINDRICAL.PS.log";
                    my $runerr_messages = $outdir . $source . "_CYLINDRICAL.PS.err";
                       $header_row = $header_row . "<td>$shell,$linktype,$odb [<b>$ps_details</b>] ".
		       "[<a href=\"$compilation_messages\">Compile log</a>,
                 <a href=\"$runout_messages\">Out</a>,
                 <a href=\"$runerr_messages\">Err</a>]:</td>";
                    my $cell       = "<td><a href=\"$ps_mnew\"><image src=\"$mnew_thumb\" alt=\"$title\"></a></td>";
                    
                    $row = $row .$cell;
                }

                $row = $row . "</tr>\n";
                $header_row = $header_row . "</tr>";
                $output_table = $output_table . $header_row . $row . $footer_row;
            }
        }
    }
    
    $output_table = $output_table . "</table>";


    


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
    $output_table
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
# Main loop - for each platform, generate the html
# -------------------------------------------------------

foreach $platform (@platforms)
{
    print ("PLATFORM: $platform\n");

    # -------------------------------------------------------
    # Inner loop - for each example program, generate the html
    # -------------------------------------------------------

    $html = "html/checkup/$platform.html";
    $/ = undef;
    open(IN,file_to_string("common/template.htmlx"));
    my $text = <IN>;
    close(IN);
    $text =~ s/(<!--\s*#BeginEditable\s+\"(.*?)\"\s*-->)(.*?)(<!--\s*#EndEditable\s*-->)/text($1,$2,$3,$4)/sge;

    open  HTML, ">" . $html;
    print HTML $text;
    close HTML;
}

