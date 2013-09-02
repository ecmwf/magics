#!/usr/bin/perl

use common::mpp_utils;

my $ecmwf      = file_to_string ("common/ecmwf.htmlx");
my @menu       = example_menu   (); 
my $page_title = "Magics++ Programming Example";
my @devices    = devices        ();
my $projection = "CYLINDRICAL";
my $max_pages  = 5;
my $gif_animation = 1; # 1 if using animated gifs, 0 if multi-page

$audience = "internal";  # temporary fix



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
	$default = $default . file_to_string("common/location-test-external.htmlx");
	$default = $default . "<a href=\"index.html\">Fortran</a>";
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
    
    my $srcfile        = "4_examples/progs/src/www/$source.f";
    my $notesfile      = "4_examples/progs/src/www/$source.txt";
    my $destdir        = "html/examples/fortran";
    my $notes_table    = "";
    my $dev_selection  = "";
    my $srccode  = fortran_file_to_string( $srcfile);
    my $pic_mnew       = $source . "_mpp_$projection" . "_med.gif";
    my $pic_mnew_thumb = $source . "_mpp_$projection" . "_small.gif";
    my $pic_mold       = $source . "_m69_$projection" . "_med.gif";
    my $pic_mold_thumb = $source . "_m69_$projection" . "_small.gif";
    my $ps_mnew        = $source . "_mpp_$projection.ps";
    my $ps_mold        = $source . "_m69_$projection.ps";
    my $m69 = ($audience eq "internal") ? 1 : 0;  # do we include the MAGICS 6.x plots?


    # if this example does not have a MAGICS6.9 output, then
    # put in dummy images instead

    if ($no69 eq "y")
    {
        $pic_mold       = "no69.gif";
        $pic_mold_thumb = "no69_small.gif";
    }



    if (-e $notesfile)
    {
        my $notes  = file_to_string($notesfile);
        
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



    # our portion of the table that gives the user access to the different output formats.
    # Just defining the HTML at the moment; we'll add it to the rest a little later.

    foreach my $device (@devices)
    {
        # do not do for PostScript (this is handled elsewhere) or for SVG (if disabled for this plot)

        if (  ($device ne 'PS')                  && 
            !(($device eq 'SVG') && $svg eq 'n') &&
            !(($device eq 'GIF') && $gif eq 'n')
           )
        {
            my $lowdev   = lc($device);
            my $outfile  = "$source" . "_mpp_${projection}.$lowdev";
            my $uselink  = 0;
            my $name     = $device;
            my $link     = "<A HREF=\"$outfile\">";


            # the first (and maybe only) page of output

            $dev_selection = $dev_selection . "
            <TR>
                <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"$device\">" . $link . " $name</A>";


            # each subsequent page, if they exist

            for (my $page = 1; $page < $max_pages; ++$page)
            {
                $uselink = 0;
                $name    = $device;
                
#                $outfile  = "$source" . "_mpp_$projection" . "_0" . "$page.$lowdev";
                $outfile  = "$source" . "_mpp_$projection" . "$page.$lowdev";

                if (-e "$destdir$outfile")
                {
                    $name    = "(Page " . ($page + 1) . ")";
                    $link    = "<A HREF=\"$outfile\">";
                    
                    $dev_selection = $dev_selection . ", $link" . " $name</A>";
                }
            }
            

            # finish up for this device

            $dev_selection = $dev_selection . "
                     </TD>";
            if ($m69)
            {
                $dev_selection = $dev_selection . "
                <TD> </TD>
                <TD> </TD>";
            }
            
            $dev_selection = $dev_selection . "
            </TR>
            ";

        }
    }






    # ----------------------------------
    # the main body of text for the page
    # ----------------------------------



    my $m69_plot = ($m69)
                   ? "        <TD> <A HREF=\"$pic_mold\"><IMG SRC=\"$pic_mold_thumb\" ALT=\"MAGICS 6.9 Plot\"></A> </TD>"
                   : "";

    my $m69_caption = ($m69)
                      ? "        <TD><I>Magics 6.9 output - click to enlarge</I></TD>"
                      : "";

    my $m69_icon = ($m69)
                   ? "        <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"MAGICS 6.9 PostScript\"> <A HREF=\"$ps_mold\">Postscript</A> </TD>"
                   : "";

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
    <CENTER>
    <TABLE>
    <TR>
        <TD> <A HREF=\"$pic_mnew\"><IMG SRC=\"$pic_mnew_thumb\" ALT=\"Magics++ Plot\"></A> </TD>
        $m69_plot
    </TR>
    <TR>
        <TD><I>Magics++ output - click to enlarge</I></TD>
        $m69_caption
    </TR>
    <TR>
        <TD> <IMG SRC=\"ps_icon.gif\" ALT=\"Magics++ PostScript\">   <A HREF=\"$ps_mnew\">Postscript</A> </TD>
        $m69_icon
    </TR>
    $dev_selection
    </TABLE>
    $notes_table
    <TABLE border = \"1\" bgcolor = \"#FFFFDD\">
    <TR>
        <TD align=\"center\"><B>Output Formats</B></TD>
    </TR>
    <TR>
        <TD>
        The output formats presented here are PostScript and GIF. The PostScript driver can also
        output PDF files. There is also an SVG driver, but SVG viewers are not yet mature.
        In order to automate the process of generating plots for multiple devices, the FORTRAN code
        has been complicated slightly by the addition of command-line parsing.
        </TD>
    </TR>
    </TABLE>
    <P>
    </CENTER>
    <HR>
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
# Main loop - for each example program, generate the html
# -------------------------------------------------------

foreach my $example (@menu)
{
	my $title   = $example->{name};	
	my $html    = $example->{html};
       $source  = $example->{source};
       $desc    = $example->{description};
       $no69    = $example->{no69};
       $svg     = $example->{svg};
       $gif     = $example->{gif};
    my $lang    = $example->{lang};
    my $fordocs = ($audience eq "internal")
                  ? $example->{fordocs}
                  : $example->{ext};


    if (($lang eq "fortran") && ($fordocs eq "yes"))
    {
        $html = "html/examples/fortran/$source.html";
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









