package common::mpp_utils;  # assumes Some/Module.pm
use Exporter;
use File::Basename;
use File::stat;

@ISA = ('Exporter');

@EXPORT = qw(&file_to_string &file_to_string_nofail &fortran_file_to_string &current_date 
             &formats_menu &intro_menu &interfaces_menu &example_menu &testsuite_submenus
             &classicsuite_submenus &classic_menu &projections &devices &all_devices &projections_info
             &magml_testsuite_menu &magml_testsuite_submenus &magml_devices &magml_file_to_string
             &test_suite_selection_html compile_menu epsgram_testsuite_menu epsgram_devices &checkup_platforms
             &checkup_testsuite_menu &c_file_to_string &c_testsuite_menu &c_testsuite_submenus &c_devices
             &filesize &program_listing &last_modified_details &check_mpp_for_completion_message &get_valgrid_summary
             &magplus_version_string &fortan_interface_menu &c_interface_menu &cpp_interface_menu &magml_interface_menu
             &eps_interface_menu &emphasis &eps_template_from_magml_to_string &epsgram_checkup_menu
             &log_file_to_string &html_embed &devices_info);







# ------------------------------------------------
#  emphasis
#  Returns the given text with HTML emphasis on it
# ------------------------------------------------

sub emphasis
{
    my $text = $_[0];
    
    return "<font color=\"#7777DD\"><b>$text</b></font>";
}






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



# ------------------------------------------------------------------------
# program_listing
# Returns a listing of the program executable (for the date/time and size)
# ------------------------------------------------------------------------

sub program_listing {

    local ($filename)  = $_[0];

    $st = stat($filename);
    
    my    $strResult = "<pre><font size=\"-2\">";

    if ($st)
    {
        # get a listing of the program

        $strResult = $strResult . `ls -lh $filename`;
    }
    else
    {
        $strResult = $strResult . "File not found";
    }

    $strResult = $strResult . "</font></pre>";

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


    $st = stat($filename);

    if ($st)
    {
        # get a listing of the program

        $strListing = `ls -lh $filename`;


        # split into individual words and store in array

        @aparts = split (/\s+/, $strListing);


        $strResult = $strResult . @aparts[5] . " " . @aparts[6];
    }
    else
    {
        $strResult = $strResult . "File not found";
    }

    return $strResult;
}




# ------------------------------------------------------------------------
# check_mpp_for_completion_message
# Checks the given log file for the Magics++ completion message. If it is
#   not there, then the program did not complete. Returns 1 (error) or 0
# ------------------------------------------------------------------------

sub check_mpp_for_completion_message {

    local($filename) = @_[0];
    local($line)   = "";
    my $line_num = 1;
    my $buffer;

    open (FILE, $filename) || return 0;  # assume no news is bad news...


    # for speed, especially when opening very large log files), we read just
    # the last 800 characters and check them for the completion string

    seek (FILE, 0, 2);                            # seek to the end of the file
    my $file_len   = tell (FILE);                 # the length of the file
    my $seek_place = ($file_len > 800) ? 800 : $file_len;
    seek (FILE, -$seek_place, 2);                 # seek to the (hopefully) start of the completion message
    my $rv = read (FILE, $buffer, $seek_place);   # read the last part of the file


    if ($buffer =~ /.*COMPLETED.*/)
    {
        return 1;
    }

    return 0;
}



# ------------------------------------------------------------------------
# get_valgrid_summary
# Checks the valgrind log file for memory leak summary information
# and returns the most important as a string
# ------------------------------------------------------------------------

sub get_valgrid_summary {

    local($filename) = @_[0];
    local($line)   = "";
    my $line_num = 1;
    my $buffer;

# example string we are trying to parse:
# ==15843==    definitely lost: 7,484,447 bytes in 96,940 blocks.


    open (FILE, $filename) || return "";


    # for speed, especially when opening very large log files), we read just
    # the last 800 characters and check them for the completion string

    seek (FILE, 0, 2);                            # seek to the end of the file
    my $file_len   = tell (FILE);                 # the length of the file
    my $seek_place = ($file_len > 800) ? 800 : $file_len;
    seek (FILE, -$seek_place, 2);                 # seek to the (hopefully) start of the completion message
    my $rv = read (FILE, $buffer, $seek_place);   # read the last part of the file


    if ($buffer =~ /.*definitely\slost:\s(\S+?)\sbytes/)
    {
        my $byteslost       = $1;
        my $bytesloststring = $1;

	    $byteslost =~ s/,//g;   # remove commas from the string
        
        if ($byteslost > 1024*500)  # highlight if high number
        {
            return "lost <b>$bytesloststring</b> bytes";
        }
        
        return "lost $bytesloststring bytes";
    }

    return "";
}


# ------------------------------------------------------------------------
# magplus_version_string
# Returns the Magics++ version string
# ------------------------------------------------------------------------

sub magplus_version_string {

    return  `magics-config --version`;
}


# ------------------------------------------------------------------------------

# file_to_string: reads a file and returns the text as a string
# exits if the file does not exist

sub file_to_string
{
	local($filename) = @_[0];
	local($string)   = ""; 

	open (FILE, $filename) || return ("Cannot open file $filename.");
# 	open (FILE, $filename) || die "cannot open file $filename.";

	@Lines = <FILE>;
	foreach $line (@Lines)
	{
		$string = $string . $line;
	}

	return ($string);
}


# ------------------------------------------------------------------------------

# file_to_string_nofail:reads a file and returns the text as a string
# returns an error message but does not exit if the file does not exist

sub file_to_string_nofail
{
	local($filename) = @_[0];
	local($string)   = ""; 

	open (FILE, $filename) || return "cannot open file $filename.";

	@Lines = <FILE>;
	foreach $line (@Lines)
	{
		$string = $string . $line;
	}

	return ($string);
}



# ------------------------------------------------------------------------------

sub fortran_file_to_string
{
    local($filename) = @_[0];
    local($string)   = "";
    local(@Lines);
    my $holdTerminator = $/;

    # in order to load the file line-by-line, we make a note of
    # the current line terminator, replace it with a proper newline,
    # then put it back to how it was before.
    
    $/ = "\n";

    open (FILE, $filename) || return ("Cannot open file $filename.");
#    open (FILE, $filename) || die "cannot open file $filename.";

    @Lines = <FILE>;
    close (FILE);

    foreach $line (@Lines)
    {
        # is the line a comment?
        # ie, does it start with a 'C' or 'c' on the first column?
        
        if ($line =~ /^[Cc]/)
        {
            $string = $string . "<font color=\"#229922\"><i>" . $line . "</i><\/font>";
        }


        # no, but is it an 'include' statement?

#        elsif ($line =~ /#include (".*")/)
        elsif ($line =~ /#include "(.*)"/)
        {
            $string = $string . fortran_file_to_string ((dirname $filename) . "/$1");
        }

        else
        {
		    # colour anything in "quotes" values

		#    $line =~ s/'([\w-\W\(\),\/\.:]*)'/<font color="#884444">'$1'<\/font>/g;
		    $line =~ s/'([\w-\W]*)'/<font color="#884444">'$1'<\/font>/g;


		    # colour the MAGICS function names

		    $line =~ s/(PSET[^(]*)/<font color="#444488"><b>$1<\/b><\/font>/g;


		    # colour the MAGICS action routine names

		    $line =~ s/(POPEN|PCLOSE|PCOAST|PCONT|PTEXT|PLEGEND|PAXIS|PGRAPH|PIMAGE|PGRIB|PNETCDF|PODB|PNEW|POBS|PSYMB|PWIND|PBOXPLOT)/<font color="#7777CC"><b>$1<\/b><\/font>/g;

            $string = $string . $line;
        }
    }

    $/ = holdTerminator;

    return ($string);
}

# ------------------------------------------------------------------------------

sub c_file_to_string
{
	local($filename) = @_[0];
	local($string)   = "";
	local(@Lines);
	my $holdTerminator = $/;

	# in order to load the file line-by-line, we make a note of
	# the current line terminator, replace it with a proper newline,
	# then put it back to how it was before.
    
	$/ = "\n";

	open (FILE, $filename) || return ("Cannot open file $filename.");
#	open (FILE, $filename) || die "cannot open file $filename.";

	@Lines = <FILE>;
	close (FILE);

	foreach $line (@Lines)
	{

		# replace '<' with '&lt;' and '>' with '&gt;'
		
		$line =~ s/</&lt;/g;
		$line =~ s/>/&gt;/g;


		# is the line a comment?
		# ie, is it between /* and */ ?
		# Ok, this method does not support multi-line comments!
        
		$line =~ s/(.*)(\/\*)(.*)(\*\/)/$1<font color="#229922"><i>$2$3$4<\/i><\/font>/g;


		# colour anything in "quotes" values

		$line =~ s/"([\w-\(\),\/\.]*)"/<font color="#884444">"$1"<\/font>/g;


		# colour function names (things before brackets)

		$line =~ s/([^(\s]*\s)\(/<b>$1<\/b>\(/g;


		# colour the preprocessor commands

		$line =~ s/(#include.*$\|#if.*$\|#define.*$\|#endif.*$)/<font color="#FF0000">$1<\/font>/g;


		# colour the MAGICS function names

		$line =~ s/(mag_[^(]*)/<font color="#444488"><b>$1<\/b><\/font>/g;


        $/ = holdTerminator;

		$string = $string . $line;
	}


	return ($string);
}


# ------------------------------------------------------------------------------

sub magml_file_to_string
{
	local($filename) = @_[0];
	local($string)   = "";
	local(@Lines);
	my $holdTerminator = $/;

	# in order to load the file line-by-line, we make a note of
	# the current line terminator, replace it with a proper newline,
	# then put it back to how it was before.
    
	$/ = "\n";

	open (FILE, $filename) || return ("Cannot open file $filename.");

	@Lines = <FILE>;
	close (FILE);

	foreach $line (@Lines)
	{

		# replace '<' with '&lt;' and '>' with '&gt;'
		
		$line =~ s/</&lt;/g;
		$line =~ s/>/&gt;/g;


		# colour any attribute names (single-quotes)

		$line =~ s/([\w]*)(\s*=\s*')/<font color="#5555AA">$1<\/font>$2/g;


		# colour any attribute values (single-quotes)

		$line =~ s/'([\w-\(\),\/\.]*)'/<font color="#448844">'$1'<\/font>/g;
        

		# colour any attribute names (double-quotes)

#		$line =~ s/([\w]*)(\s*=\s*")/<font color="#5555AA">$1<\/font>$2/g;


		# colour any attribute values (double-quotes)

#		$line =~ s/"([\w-\(\),\/\.]*)"/<font color="#448844">&quot;$1&quot;<\/font>/g;


		# highlight comments

		$line =~ s/(&lt;!--.*--&gt;)/<font color="#226622"><i>$1<\/i><\/font>/g;


		# highlight tags

		$line =~ s/(&lt;[\w\/]*)/<font color="#992222">$1<\/font>/g;
		$line =~ s/(&gt;)/<font color="#992222">$1<\/font>/g;



		 $/ = holdTerminator;
		
		 $string = $string . $line;
	}

	return ($string);
}


# ------------------------------------------------------------------------------

# eps_template_from_magml_to_string
#
# parses the given MagML file, determines the name of the EPSgram template used
# (if any) and loads the template file, formatted in MagML style.

sub eps_template_from_magml_to_string
{
	local($filename) = @_[0];
	local($srddir  ) = @_[1];

    my $magml = file_to_string_nofail ($filename);
    print ("filename:" . $filename);
    
    
#    if ($magml =~ m/.*template='(.*)'.*/)
    if ($magml =~ m/.*[\w]*template='(\S[^']*)'\s.*/)
    {
        return magml_file_to_string ("$srddir/$1");
    }
    
    else
    {
        print ("Could not find eps template in $filename\n");
    }
}


# ------------------------------------------------------------------------------

# this highlights any line with ERROR in red

sub log_file_to_string
{
	local($filename) = @_[0];
	local($string)   = "";
	local(@Lines);
	my $holdTerminator = $/;

	# in order to load the file line-by-line, we make a note of
	# the current line terminator, replace it with a proper newline,
	# then put it back to how it was before.
    
	$/ = "\n";

	open (FILE, $filename) || return ("Cannot open file $filename.");

	@Lines = <FILE>;
	close (FILE);

	foreach $line (@Lines)
	{
		# highlight ERROR in red
        
		$line =~ s/(ERROR.*$)/<b><font color="#FF0000">$1<\/font><\/b>/g;


		$/ = holdTerminator;

		$string = $string . $line;
	}


	return ($string);
}



# ----------------------------------------------------------
#  html_embed
#  Returns the HTML code for embedding a given type of image
# ----------------------------------------------------------

sub html_embed
{
    my $format = $_[0];
    my $file   = $_[1];
    my $width  = $_[2];

    if    ($format eq "img") {return "<img src=\"$file\" width=\"$width\">";}
    elsif ($format eq "svg") {return "<object data=\"$file\" width=\"$width\" type=\"image/svg+xml\"\></object>";}
    else                     {return "No embedded image";}
}




# ------------------------------------------------------------------------------

sub current_date
{
	my ($default) = "";

	# retrieve the current date

	($Second, $Minute, $Hour, $Day, $Month, $Year, $WeekDay, $DayOfYear, $IsDST) = localtime(time);
	
	my $RealMonth = $Month + 1; # Months of the year are not zero-based	
	
	if($RealMonth < 10)         # adjust for one-digit months, adding a leading zero 
	{
   		$RealMonth = "0" . $RealMonth;
	}
	
	if($Day < 10)
	{
	   $Day = "0" . $Day; # add a leading zero to one-digit days
	}
	
	
	$Fixed_Year = $Year + 1900;

	$result = "$Day.$RealMonth.$Fixed_Year";
	return $result;
}

# ------------------------------------------------------------------------------

sub formats_menu
{
	local ($audience) = @_[0];
    my @f_menu;

    if ($audience eq "internal")
    {
        @f_menu = (
            { name => "PS/EPS/PDF", html => "ps.html",          type => "output", description => "PS/EPS/PDF Output Formats"},
            { name => "GD",         html => "gd.html",          type => "output", description => "GD Driver for GIF, PNG and JPEG Output Formats"},
            { name => "SVG",        html => "svg.html",         type => "output", description => "SVG Output Format"},
            { name => "Multiple",   html => "multioutput.html", type => "output", description => "Multiple Output Formats"},
        );
    }
    else
    {
        @f_menu = (
            { name => "PS/EPS/PDF", html => "ps.html",          type => "output", description => "PS/EPS/PDF Output Formats"},
            { name => "GD",         html => "gd.html",          type => "output", description => "GD Driver for GIF, PNG and JPEG Output Formats"},
            { name => "Multiple",   html => "multioutput.html", type => "output", description => "Multiple Output Formats"},
        );
    }

    return @f_menu;
}


sub intro_menu
{
    local ($audience) = @_[0];
    my @i_menu;

    if ($audience eq "internal")
    {
        @i_menu = (
	        { name => "Features",         html => "features_now.html", description => "Features currently implemented in Magics++"},
	        { name => "Changes",          html => "changes.html",      description => "Changes from MAGICS 6"},
	        { name => "Contouring",       html => "akima.html",        description => "Description of the new contouring package"},
	        { name => "Coastlines",       html => "coastlines.html",   description => "Coastlines in Magics++"},
	        { name => "Workshop Talk",    html => "index.html",        description => "Workshop Documents"},
        );
    }
    else
    {
        @i_menu = (
	        { name => "Workshop Talk",    html => "index.html",        description => "Workshop Documents"},
	        { name => "Contouring",       html => "akima.html",        description => "Description of the new contouring package"},
	        { name => "Coastlines",       html => "coastlines.html",   description => "Coastlines in Magics++"},
        );
    }

    return @i_menu;
}


sub interfaces_menu
{
    my @in_menu = (
        { name => "FORTRAN", html => "fortran/index.html", description => "FORTRAN Programming Interface"},
        { name => "C",       html => "c/index.html",       description => "C Programming Interface"},
#        { name => "C++",     html => "cpp/index.html",     description => "C++ Programming Interface"},
        { name => "MagML",   html => "magml/index.html",   description => "XML Programming Interface (MagML)"},
#        { name => "Metgram", html => "eps/index.html",     description => "XML-based Interface for producing metgrams"},
    );

    return @in_menu;
}


sub compile_menu
{
    my @c_menu = (
        { name => "Compilation", html => "index.html",            description => "Compiling Magics++"},
        { name => "ODB Support", html => "compile_with_odb.html", description => "ODB Support"},
    );

    return @c_menu;
}



sub fortan_interface_menu
{
    my @f_in_menu;

    if ($audience eq "internal")
    {
        @f_in_menu = (
            { name => "Introduction", html => "index.html",                               description => "FORTRAN Programming Interface"},
            { name => "Compilation",  html => "fortran_compile.html",                     description => "Compilation"},
            { name => "ODB Support",  html => "fortran_compile_with_odb.html",            description => "ODB Support"},
            { name => "Parameters",   html => "../../parameters/fortranactionindex.html", description => "FORTRAN Parameters"},
            { name => "Examples",     html => "../../examples/fortran/index.html",        description => "Examples"},
        );
    }
    else
    {
        @f_in_menu = (
            { name => "Introduction", html => "index.html",                               description => "FORTRAN Programming Interface"},
            { name => "Parameters",   html => "../../parameters/fortranactionindex.html", description => "FORTRAN Parameters"},
            { name => "Examples",     html => "../../examples/fortran/index.html",        description => "Examples"},
        );
    }

    return @f_in_menu;
}

sub c_interface_menu
{
    my @c_in_menu = (
        { name => "Introduction", html => "index.html",                  description => "C Programming Interface"},
        { name => "Examples",     html => "../../examples/c/index.html", description => "Examples"},
    );

    return @c_in_menu;
}

sub cpp_interface_menu
{
    my @cpp_in_menu = (
        { name => "Introduction", html => "index.html",   description => "C++ Programming Interface"},
    );

    return @cpp_in_menu;
}

sub magml_interface_menu
{
    my @magml_in_menu = (
        { name => "Introduction",        html => "index.html",                       description => "MagML Programming Interface"},
        { name => "Tags and Attributes", html => "outline.html",                     description => "MagML Tags and Attributes"},
        { name => "Examples",            html => "../../examples/magml/index.html",  description => "MagML Examples"},
    );

    return @magml_in_menu;
}


sub eps_interface_menu
{
    my @eps_in_menu = (
        { name => "Introduction", html => "index.html",   description => "Producing EPSgrams with Magics++"},
    );

    return @eps_in_menu;
}


sub testsuite_submenus
{
    my @ts_subs_menu = (
        { name => "Basic"      ,  first => "coastonly_1"},
        { name => "Data"       ,  first => "cont_array"},
        { name => "Contouring" ,  first => "cont_noshade_ex"},
        { name => "ContStyles" ,  first => "cont_dotshade_test1"},
        { name => "Coastlines" ,  first => "coast_res"},
        { name => "Image"      ,  first => "image_basic"},
        { name => "Layout"     ,  first => "layout_auto_ex"},
        { name => "Observations", first => "obs_ex"},
        { name => "ODB"        ,  first => "odb_t2m_ex"},
        { name => "Text"       ,  first => "text_ex"},
        { name => "Graph"      ,  first => "graf01"},
        { name => "Axis"       ,  first => "axis01"},
        { name => "Wind"       ,  first => "wind_ex"},
        { name => "MetOps"     ,  first => "metops_t_and_z"},
        { name => "External"   ,  first => "ukmo_netcdf_01"},
    );

    return @ts_subs_menu;
}



sub example_menu
{
    #                                                                              category                                internal www     external www                                       
	my @ex_menu = ( 
 	{ name => "No Plot",             source => "noplot",                     cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Open and Close MAGICS, no plotting"},
 	{ name => "Coast Grid Only",     source => "coastonly_1",                cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "No data, just coast grid"},
 	{ name => "Coastline Only",      source => "coastonly_2",                cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "No data, just coastline"},
 	{ name => "Linear Contour Only", source => "dataonly_linear",            cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Only data (LINEAR contour), no coastline"},
 	{ name => "Import",              source => "import_1",                   cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Import and plot images"},
 	{ name => "Multiple Projections",source => "multiple_projections",       cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Multiple projections"},
 	{ name => "PENQ 1",              source => "penq_1",                     cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "PENQ test 1"},
 	{ name => "Land/Sea Shading",    source => "landsea_shading",            cat => "Coastlines",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Land and Sea Shading"},
 	{ name => "Map Boundaries",      source => "coast_boundaries",           cat => "Coastlines",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Map Boundaries"},
 	{ name => "Large Area",          source => "views_large_area",           cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Area greater than 360 degrees"},
 	{ name => "PPRINT",              source => "basic_pprint",               cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "PPRINT action routine"},
 	{ name => "Netcdf Data Plotting",source => "netcdf_ex",                  cat => "Data",         critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "y", svg => "n", gif => "y", drivers => "n",description => "Netcdf Data Plotting"},
 	{ name => "Netcdf - More",       source => "data_netcdf2",               cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "y", svg => "n", gif => "y", drivers => "n",description => "Netcdf Data Plotting (large file)"},
 	{ name => "Array Data Contouring", source => "cont_array",               cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Array Data Contouring"},
 	{ name => "Array Data Wind",     source => "wind_vector_array",          cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Array Data Wind Plotting"},
 	{ name => "Array Data Wind",     source => "wind_polar_array",           cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Array Data Wind Plotting (Polar)"},
 	{ name => "Invalid GRIB indicator", source => "data_invalid_values",     cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Masked data to test invalid GRIB indicator"},
 	{ name => "T799 High Res",       source => "data_t799",                  cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of 0.225 degree ll t799 grid"},
 	{ name => "T799 T2m High Res",   source => "data_t799_t2m",              cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of 0.225 degree ll t799 grid (T2m)"},
 	{ name => "T1279 GG640",         source => "data_t1279_gg640",           cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of gg640 t1279 grid"},
 	{ name => "T1279 GG640 Cell",    source => "data_t1279_gg640_cell",      cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of gg640 t1279 grid with cell shading"},
 	{ name => "T1279 GG640 UK",      source => "data_t1279_gg640_uk",        cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of gg640 t1279 grid on UK area"},
 	{ name => "Reduced GG640",       source => "data_rgg640",                cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of reduced gg640 grid "},
 	{ name => "Reduced GG2000",      source => "data_t3999_rgg2000",         cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of reduced gg2000 grid "},
 	{ name => "Reduced Lat/long",    source => "data_rll_1-deg",             cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of reduced lat/long grid "},
 	{ name => "GRIB Sources",        source => "data_gribs",                 cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of various GRIB files"},
 	{ name => "GRIB 2",              source => "data_grib2",                 cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of GRIB 2 files"},
 	{ name => "Gaussian Grid",       source => "data_gaussian",              cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of Gaussian grid GRIB files"},
 	{ name => "Limited Area",        source => "data_limited_area",          cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of limited area GRIB files"},
 	{ name => "Hirlam",              source => "data_hirlam",                cat => "Data",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Test of Hirlam rotated GRIB files"},
 	{ name => "Contours",            source => "cont_noshade_ex",            cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "y",description => "Basic Contouring, no shading"},
 	{ name => "Solid",               source => "cont_solidshade_ex",         cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Basic Contouring, solid shading"},
 	{ name => "Solid 1",             source => "cont_solidshade_test1",      cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Solid Shading area tests (z500)"},
 	{ name => "Solid 2",             source => "cont_solidshade_test2",      cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Solid Shading area tests (rh850)"},
 	{ name => "Solid 3",             source => "cont_solidshade_test3",      cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Solid Shading area tests (lsp25)"},
 	{ name => "Grid Shade",          source => "cont_gridshade_test1",       cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Grid Shading (solid)area tests (lsp25)"},
 	{ name => "Dot",                 source => "cont_dotshade_test1",        cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Dot Shading area tests (z500)"},
 	{ name => "Hatch",               source => "cont_hatchshade_ex",         cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Basic Contouring, hatch shading"},
 	{ name => "Marker",              source => "cont_markershade_ex",        cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Basic Contouring, marker shading"},
 	{ name => "Cell",                source => "cont_cellshade_test1",       cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "no",  ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Basic Contouring, cell shading"},
 	{ name => "Grid",                source => "cont_gridshade_test1",       cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "no",  ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Basic Contouring, grid shading"},
 	{ name => "Split Contour",       source => "cont_split",                 cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Split contour"},
 	{ name => "Grid Values",         source => "cont_gridvals_ex",           cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring with grid values (subarea)"},
 	{ name => "Missing Vals",        source => "cont_missing",               cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring with missing values"},
 	{ name => "Missing (Montreal)",  source => "cont_missing_2",             cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring with missing values (Montreal data)"},
 	{ name => "Flat Field 1",        source => "cont_flat_field_1",          cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring a fairly flat field"},
 	{ name => "EFOV",                source => "cont_EFOV",                  cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "VarEPS (EFOV)"},
 	{ name => "Line Styles",         source => "cont_linestyles_ex",         cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "yes", ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring with different line styles"},
 	{ name => "Methods",             source => "cont_methods_ex",            cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring using different methods"},
 	{ name => "Resolutions",         source => "cont_res_ex",                cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring using different resolutions"},
 	{ name => "Attributes",          source => "cont_attributes_ex",         cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contour attributes"},
# 	{ name => "Contour Attributes 2",source => "cont_attributes_manual_ex",  cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "y", gif => "y", drivers => "n",description => "Contour attributes (manual layout)"},
 	{ name => "Labels",              source => "cont_labels",                cat => "ContStyles",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "y", gif => "y", drivers => "n",description => "Contour labels"},
 	{ name => "Alternate Contours",  source => "cont_alternate",             cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Contouring alternate bands"},
 	{ name => "Automatic Contour",   source => "cont_auto_ex",               cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Automatic contour method selection"},
 	{ name => "Automatic (T799)",    source => "cont_auto_t799",             cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "yes",no69 => "n", svg => "y", gif => "y", drivers => "n",description => "Automatic contour (T799)"},
 	{ name => "Automatic (T1279)",   source => "cont_auto_t1279",            cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "yes",no69 => "n", svg => "y", gif => "y", drivers => "n",description => "Automatic contour (T1279)"},
 	{ name => "Akima Example",       source => "cont_akima_example",         cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Simple Akima example (used in slides)"},
# 	{ name => "LINEAR Contour Tests",source => "cont_linear_res",            cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Automatic contour parameters"},
 	{ name => "CONT08",              source => "cont08",                     cat => "Contouring",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "CONT08 example"},
 	{ name => "Satellite Image",     source => "image_basic",                cat => "Image",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no ",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Basic satellite image example"},
 	{ name => "Satellite Projections",source => "image_projs",               cat => "Image",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no ",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Satellite imagery under different projections"},
 	{ name => "Satellites",          source => "image_sats",                 cat => "Image",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no ",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Different satellites"},
 	{ name => "Met 9 Zoomed",        source => "image_met9_zoomed",          cat => "Image",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no ",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Zoomed areas of Meteosat 9"},
 	{ name => "Met 9 Zoomed (New)",  source => "image_met9_post_oct2007",    cat => "Image",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no ",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Zoomed areas of Meteosat 9 (post 12th Oct 2007)"},
 	{ name => "Auto Page Layout",    source => "layout_auto_ex",             cat => "Layout",       critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Automatic Page layout example"},
 	{ name => "Manual Page Layout",  source => "layout_manual_ex",           cat => "Layout",       critial => "n", lang => "fortran", fordocs => "yes", ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Manual Page layout example"},
 	{ name => "Manual Layout 2",     source => "layout_manual_2",            cat => "Layout",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Simple Manual Page layout example"},
 	{ name => "6 Plots",             source => "layout_6_plots",             cat => "Layout",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "6 Plots on a page"},
 	{ name => "User Logo",           source => "logo",                       cat => "Layout",       critial => "n", lang => "fortran", fordocs => "yes", ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "User logo example"},
 	{ name => "User Logo with Data", source => "logo_with_data",             cat => "Layout",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "User logo with data"},
 	{ name => "Logo With Layout",    source => "logo_layout",                cat => "Layout",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Logos under different layouts"},
 	{ name => "Transparent White",   source => "transparent",                cat => "Layout",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "GD_TRANSPARENT set to ON"},
 	{ name => "Coastline Res Tests", source => "coast_res",                  cat => "Coastlines",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Coastline resolution visualisation"},
 	{ name => "Coastline Attributes",source => "coast_attributes_ex",        cat => "Coastlines",   critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Coastline attributes"},
 	{ name => "Observation Plotting",source => "obs_ex",                     cat => "Observations", critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Observation Plotting"},
 	{ name => "ODB 2m T Example",    source => "odb_t2m_ex",                 cat => "ODB",          critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "y", svg => "n", gif => "y", drivers => "n",description => "ODB (2m Temperature)"},
 	{ name => "ODB AIRS Example",    source => "odb_airs_ex",                cat => "ODB",          critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "y", svg => "n", gif => "y", drivers => "n",description => "ODB (AIRS)"},
 	{ name => "Polar Stereographic", source => "proj_polarstereo_ex",        cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Polar Stereographic projection"},
 	{ name => "Polar Stereo Solid",  source => "proj_polarstereo_solid",     cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Polar Stereographic, solid shading"},
 	{ name => "Missing GRIB file",   source => "errors_missinggrib",         cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Missing GRIB data file"},
 	{ name => "Bad Parameter",       source => "errors_badparam",            cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Bad Parameter"},
 	{ name => "Wrong Parameter Type", source => "errors_wrongparamtype",     cat => "Basic",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wrong Parameter Type"},
 	{ name => "Wind Plotting",       source => "wind_ex",                    cat => "Wind",         critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wind Plotting"},
 	{ name => "Wind Arrows",         source => "wind_arrows",                cat => "Wind",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wind Arrows"},
 	{ name => "Wind Arrows [New]",   source => "wind_arrows_new",            cat => "Wind",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wind Arrows [New]"},
 	{ name => "Wind (Vor/Div)",      source => "wind_vo_d",                  cat => "Wind",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wind (Vorticity/Divergence)"},
 	{ name => "Wind / land shading", source => "wind_landshade",             cat => "Wind",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wind with land shading"},
 	{ name => "Wind Test",           source => "wind_test",                  cat => "Wind",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wind Test"},
 	{ name => "Symbol Table",        source => "symbol_table",               cat => "Observations", critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Symbol Table"},
 	{ name => "Symb02",              source => "symb02",                     cat => "Observations", critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Symb02"},
 	{ name => "Graf01",              source => "graf01",                     cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Graf01"},
 	{ name => "Graf02",              source => "graf02",                     cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Graf02"},
 	{ name => "Graf03",              source => "graf03",                     cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Graf03"},
 	{ name => "Graf04",              source => "graf04",                     cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Graf04"},
 	{ name => "Graph Venice",        source => "graph_venice_01",            cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Graph Venice"},
 	{ name => "Scatterplot Venice",  source => "graph_venice_02",            cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Scatterplot Venice"},
 	{ name => "Boxplot",             source => "boxplot_ex",                 cat => "Graph",        critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "y", svg => "n", gif => "y", drivers => "n",description => "Boxplot Examples"},
 	{ name => "Taylor",              source => "taylor_ex",                  cat => "Graph",        critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "y", svg => "n", gif => "y", drivers => "n",description => "Taylor Diagram Examples"},
 	{ name => "Lines",               source => "graph_lines",                cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "y", svg => "n", gif => "y", drivers => "n",description => "Line Styles/Thicknesses"},
 	{ name => "PolyLines",           source => "polyline_1",                 cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "y", svg => "y", gif => "y", drivers => "n",description => "Geographical polylines"},
 	{ name => "EPS Time Series",     source => "graph_eps_tseries",          cat => "Graph",        critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "y", gif => "y", drivers => "n",description => "EPS Time Series"},
 	{ name => "Axis01",              source => "axis01",                     cat => "Axis",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Axis01"},
 	{ name => "Axis02",              source => "axis02",                     cat => "Axis",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Axis02"},
 	{ name => "Text",                source => "text_ex",                    cat => "Text",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Text Plotting"},
 	{ name => "GRIB Titles",         source => "text_grib_titles_ex",        cat => "Text",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "GRIB titles"},
 	{ name => "Fonts",               source => "text_font_tables",           cat => "Text",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Text Font Tables (for MAGOCS 6.x)"},
 	{ name => "Text01",              source => "text01",                     cat => "Text",         critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Text01"},
 	{ name => "T and Z",             source => "metops_t_and_z",             cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "yes", ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "T850 with Z500 overlaid"},
 	{ name => "MSL/Precip",          source => "metops_msl_and_precip",      cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "MSL with Precipitation"},
 	{ name => "MSL/T",               source => "metops_msl_and_t",           cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "MSL with 850 hPa Temperature"},
 	{ name => "Z/T",                 source => "metops_p12zt500",            cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Z 500 with 850 hPa Temperature"},
 	{ name => "Precip",              source => "metops_plspcpsn",            cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Precipitation"},
 	{ name => "2mT/30m Wind",        source => "metops_t2_and_wind",         cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "yes", ext => "yes",no69 => "n", svg => "n", gif => "y", drivers => "n",description => "2m T with Wind (30m)"},
 	{ name => "Cloud Cover",         source => "metops_cloudcover",          cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "yes", ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Cloud Cover"},
 	{ name => "Cluster Tubes (Old)", source => "metops_cluster_tube_old",    cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Cluster Tubes (Old)"},
 	{ name => "Cluster Tubes (New)", source => "metops_cluster_tube_new",    cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Cluster Tubes (New)"},
 	{ name => "Wave Height",         source => "metops_wave_height",         cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Wave Height"},
 	{ name => "Weather Risk",        source => "metops_weather_risk",        cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Weather Risk"},
 	{ name => "Ensemble Stamps",     source => "metops_enplot",              cat => "MetOps",       critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Ensemble Stamps"},
 	{ name => "Wind Venice",         source => "wind_venice",                cat => "External",     critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "Venice wind test"},
 	{ name => "UKMO netCDF 01",      source => "ukmo_netcdf_01",             cat => "External",     critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "UKMO netCDF Test 01"},
 	{ name => "UKMO netCDF 02",      source => "ukmo_netcdf_02",             cat => "External",     critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "UKMO netCDF Test 02"},
 	{ name => "UKMO netCDF 03",      source => "ukmo_netcdf_03",             cat => "External",     critial => "n", lang => "fortran", fordocs => "no",  ext => "no", no69 => "n", svg => "n", gif => "y", drivers => "n",description => "UKMO netCDF Test 03"},
	) ;
		
	return @ex_menu;
}





# set of examples from the 'classic' test suite

sub classicsuite_submenus
{
    my @classic_subs_menu = (
        { name => "Axis"        ,  first => "axis01"},
        { name => "Contour"     ,  first => "cont01"},
        { name => "Data"        ,  first => "data01"},
        { name => "Graph"       ,  first => "graf01"},
        { name => "Image"       ,  first => "image01"},
        { name => "Legend"      ,  first => "legend01"},
        { name => "Obs"         ,  first => "obs01"},
        { name => "Plot"        ,  first => "plot01"},
        { name => "Stream"      ,  first => "strm01"},
        { name => "Symbol"      ,  first => "symb01"},
        { name => "Text"        ,  first => "text01"},
        { name => "Wind"        ,  first => "wind01"},
    );

    return @classic_subs_menu;
}


sub classic_menu
{
	my @cls_menu = ( 
 		{ source => "axis01",   cat => "Axis",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "axis02",   cat => "Axis",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont01",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont02",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont03",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont04",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont05",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont06",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont07",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont08",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont09",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont10",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont11",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "cont12",   cat => "Contour",   mpp => "y", svg => "n", gif => "y"},
 		{ source => "data01",   cat => "Data",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "data02",   cat => "Data",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "data03",   cat => "Data",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "data04",   cat => "Data",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "data05",   cat => "Data",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "data06",   cat => "Data",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "graf01",   cat => "Graph",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "graf02",   cat => "Graph",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "graf03",   cat => "Graph",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "graf04",   cat => "Graph",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image01",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image02",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image03",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image04",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image05",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image06",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image07",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "image08",  cat => "Image",     mpp => "y", svg => "n", gif => "y"},
 		{ source => "legend01", cat => "Legend",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "legend02", cat => "Legend",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "legend03", cat => "Legend",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "legend04", cat => "Legend",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "obs01",    cat => "Obs",       mpp => "y", svg => "n", gif => "y"},
 		{ source => "obs02",    cat => "Obs",       mpp => "y", svg => "n", gif => "y"},
 		{ source => "obs03",    cat => "Obs",       mpp => "y", svg => "n", gif => "y"},
 		{ source => "obs04",    cat => "Obs",       mpp => "y", svg => "n", gif => "y"},
 		{ source => "plot01",   cat => "Plot",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "plot02",   cat => "Plot",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "plot03",   cat => "Plot",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "plot04",   cat => "Plot",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "strm01",   cat => "Stream",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "strm02",   cat => "Stream",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "symb01",   cat => "Symbol",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "symb02",   cat => "Symbol",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "symb03",   cat => "Symbol",    mpp => "y", svg => "n", gif => "y"},
 		{ source => "text01",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text02",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text03",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text04",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text05",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text06",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text07",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "text08",   cat => "Text",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "wind01",   cat => "Wind",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "wind02",   cat => "Wind",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "wind03",   cat => "Wind",      mpp => "y", svg => "n", gif => "y"},
 		{ source => "wind04",   cat => "Wind",      mpp => "y", svg => "n", gif => "y"},
		) ;
		
	return @cls_menu;
}



# ----------------------------------------------------------------------
# projections
# Returns a list of all the projections that are used in the test suite
# ----------------------------------------------------------------------

sub projections
{
    my @projections_list = ("CYLINDRICAL", "POLAR_STEREOGRAPHIC", "MERCATOR");
    
    return @projections_list;
}


# ------------------------------------------------------------
# projections_info
# Returns a list of information - one hash for each projection
# ------------------------------------------------------------

sub projections_info
{
    my %projections_info_hash = (

        "MERCATOR" =>
        {
            shortname => "mercator",
            bgcolour  => "#FFEEEE",
        },

        "POLAR_STEREOGRAPHIC" =>
        {
            shortname => "polar",
            bgcolour  => "#EEFFEE",
        },

        "CYLINDRICAL" =>
        {
            shortname => "cylindrical",
            bgcolour  => "#EEEEFF",
        },
    );

    return %projections_info_hash;
}



# ----------------------------------------------------------------------
# devices
# Returns a list of all the devices that are used in the test suite
# ----------------------------------------------------------------------

sub devices
{
    my @devices_list = ("PS", "SVG", "GIF", "PNG");
    
    return @devices_list;
}


# -------------------------------------------------------------------------
# all_devices
# Returns a list of all the devices that are used in the drivers test suite
# -------------------------------------------------------------------------

sub all_devices
{
#    my @devices_list = ("PS", "GIF", "SVG", "PDF", "JPEG", "EPS", "PNG", "KML");
    my @devices_list = ("PS", "SVG", "PDF", "EPS", "PNG", "KML", "GIF");
    
    return @devices_list;
}


# -------------------------------------------------------------------------
# devices_info
# Returns a list of information on all the available output devices 
# -------------------------------------------------------------------------

sub devices_info
{

# categories:
#  multifile: "y" if multiple output files can be produced from a single run

	my %devices_info_list = ( 
 		PS   => { multifile => "n", embedtag => "",     toprow => "n"},
 		PDF  => { multifile => "n", embedtag => "",     toprow => "n"},
 		EPS  => { multifile => "y", embedtag => "",     toprow => "n"},
 		GIF  => { multifile => "n", embedtag => "img",  toprow => "n"},
 		SVG  => { multifile => "y", embedtag => "svg",  toprow => "n"},
 		JPEG => { multifile => "y", embedtag => "img",  toprow => "n"},
 		PNG  => { multifile => "y", embedtag => "img",  toprow => "y"},
 		KML  => { multifile => "n", embedtag => "",     toprow => "n"},
		) ;

		
#	my @devices_info_list = ( 
# 		{ name => "PS",   multifile => "n"},
# 		{ name => "GIF",  multifile => "n"},
# 		{ name => "SVG",  multifile => "n"},
# 		{ name => "PDF",  multifile => "n"},
# 		{ name => "JPEG", multifile => "n"},
# 		{ name => "EPS",  multifile => "n"},
# 		{ name => "PNG",  multifile => "n"},
# 		{ name => "KML",  multifile => "n"},
#		) ;

	return %devices_info_list;
}



sub magml_testsuite_menu
{
    #                                                                               category                                internal www     external www                                       
	my @magml_tests_menu = ( 
 		{ name => "Coast grid only",      source => "coastonly_1",                  cat => "Basic",        lang => "magml", fordocs => "no",  ext => "no",  description => "Coast grid only"},
 		{ name => "Coast and grid only",  source => "coastonly_2",                  cat => "Basic",        lang => "magml", fordocs => "yes", ext => "yes", description => "Coast and grid only"},
 		{ name => "Simple Contour",       source => "contour_simple",               cat => "Basic",        lang => "magml", fordocs => "yes", ext => "yes", description => "Simple contour plotting with MagML"},
 		{ name => "Simple Contour [v]",   source => "contour_simple_v",             cat => "Basic",        lang => "magml", fordocs => "yes", ext => "yes", description => "Simple contour plotting with variables"},
 		{ name => "Field overlays",       source => "contour_tzuv",                 cat => "Basic",        lang => "magml", fordocs => "yes", ext => "yes", description => "Overlay fields from one grib file"},
 		{ name => "Hirlam",               source => "hirlam",                       cat => "Basic",        lang => "magml", fordocs => "yes", ext => "yes", description => "Hirlam rotated grid"},
 		{ name => "Contour Line Styles",  source => "contour_line_styles",          cat => "Contour",      lang => "magml", fordocs => "no",  ext => "no",  description => "Contour Line Styles"},
 		{ name => "Contour Shade Styles", source => "contour_shade_styles",         cat => "Contour",      lang => "magml", fordocs => "no",  ext => "no",  description => "Contour Shade Styles"},
 		{ name => "Shaded Contour",       source => "contour_shaded",               cat => "Contour",      lang => "magml", fordocs => "no",  ext => "no",  description => "Shaded Contour and Coastlines"},
 		{ name => "Multipage 1",          source => "multipage_1",                  cat => "Layout",       lang => "magml", fordocs => "no",  ext => "no",  description => "Multiple pages with MagML"},
 		{ name => "Axis Test",            source => "axistest",                     cat => "Axis",         lang => "magml", fordocs => "no",  ext => "no",  description => "Cartesian Axis Test"},
 		{ name => "Taylor Diagram 1",     source => "taylor_1",                     cat => "Axis",         lang => "magml", fordocs => "no",  ext => "no",  description => "Taylor Diagram Test 1"},
 		{ name => "Taylor Diagram 2",     source => "taylor_2",                     cat => "Axis",         lang => "magml", fordocs => "no",  ext => "no",  description => "Taylor Diagram Test 2"},
 		{ name => "Nils Plot",            source => "nils",                         cat => "Research",     lang => "magml", fordocs => "no",  ext => "no",  description => "Nils' Plot"},
 		{ name => "Hovmoeller",           source => "hovmoeller",                   cat => "Research",     lang => "magml", fordocs => "no",  ext => "no",  description => "Nils' Hovmoeller Plot"},
 		{ name => "MSL/T",                source => "metops_msl_and_t",             cat => "MetOps",       lang => "magml", fordocs => "yes", ext => "yes", description => "MSL with 850 hPa Temperature"},
 		{ name => "Wind 925",             source => "plot_wind_925_pl",             cat => "Baudouin",     lang => "magml", fordocs => "yes", ext => "no",  description => "6 Global Wind fields"},
 		{ name => "Soil Temp",            source => "plot_soil_temperature_0_sfc",  cat => "Baudouin",     lang => "magml", fordocs => "yes", ext => "no",  description => "Soil Temperature"},
 		{ name => "Cloud Cover",          source => "plot_total_cloud_cover_0_sfc", cat => "Baudouin",     lang => "magml", fordocs => "yes", ext => "no",  description => "Total Cloud Cover"},
		) ;
		
	return @magml_tests_menu;
}


sub magml_testsuite_submenus
{
    my @magml_ts_subs_menu = (
        { name => "Basic"      ,  first => "contour_simple"},
        { name => "Contour"    ,  first => "contour_line_styles"},
        { name => "Layout"     ,  first => "multipage_1"},
        { name => "Axis"       ,  first => "axistest"},
        { name => "Research"   ,  first => "nils"},
        { name => "MetOps"     ,  first => "metops_msl_and_t"},
        { name => "Baudouin"   ,  first => "plot_wind_925_pl"},
    );

    return @magml_ts_subs_menu;
}



# -----------------------------------------------------------------------
# magml_devices
# Returns a list of all the devices that are used in the MagML test suite
# -----------------------------------------------------------------------

sub magml_devices
{
    my @magml_devices_list = ("ps", "gif");
    
    return @magml_devices_list;
}



sub epsgram_testsuite_menu
{
    #                                  source file                                 category          description                                  
	my @epsgram_tests_menu = ( 
 		{ name => "Reading",         source => "eps-reading",                          cat => "Basic",   description => "Reading"},
 		{ name => "Reading-15",      source => "eps-reading-15",                       cat => "Basic",   description => "Reading 15 day"},
 		{ name => "A-B-C",           source => "eps-addis_ababa-brussels-canberra",    cat => "Basic",   description => "3 Cities"},
 		{ name => "A-B-C-15",        source => "eps-addis_ababa-brussels-canberra-15", cat => "Basic",   description => "3 Cities 15 day"},
 		{ name => "Madrid A3",       source => "eps-madrid-a3",                        cat => "Basic",   description => "Madrid A3"},
 		{ name => "Madrid-15 A3",    source => "eps-madrid-15-a3",                     cat => "Basic",   description => "Madrid A3 15 day"},
 		{ name => "Metgram-reading", source => "metgram-reading",                      cat => "Basic",   description => "Reading Metgram"},
 		{ name => "Wave-10",         source => "wave-10",                              cat => "Basic",   description => "Wavegram"},
		) ;
		
	return @epsgram_tests_menu;
}


# -------------------------------------------------------------------------
# epsgram_devices
# Returns a list of all the devices that are used in the EPSgram test suite
# -------------------------------------------------------------------------

sub epsgram_devices
{
    my @epsgram_devices_list = ("ps", "gif");
    
    return @epsgram_devices_list;
}



# -------------------------------------------------------------------------
# checkup_platforms
# Returns a list of all the platforms we run the checkup test suite on
# -------------------------------------------------------------------------

sub checkup_platforms
{
    my @checkup_platforms_list = ("suse9", "suse11", "cluster32", "cluster64", "lxab", "ibm");
    
    return @checkup_platforms_list;
}



sub checkup_testsuite_menu
{
    #                                      source file                        category          description                                  
	my @checkup_tests_menu = ( 
# 		{ name => "Contour",               source => "cont_noshade_ex",       cat => "Basic",   description => "Unshaded contours"},
        { name => "Cluster Tubes (New)",   source => "metops_cluster_tube_new", cat => "MetOps",  description => "Cluster Tubes (New)"},
        { name => "Metops T and Wind",     source => "metops_t2_and_wind",      cat => "Basic",   description => "2mT (shaded) and Wind"},
		) ;
		
	return @checkup_tests_menu;
}



sub epsgram_checkup_menu
{
    #                                    source file                  category          description                                  
	my @epsgram_checks_menu = ( 
 		{ name => "Reading",     source => "eps-reading",     cat => "Basic",   description => "Reading"},
		) ;
		
	return @epsgram_checks_menu;
}




sub c_testsuite_menu
{
    #                                                                             category                                internal www   
	my @c_tests_menu = ( 
 		{ name => "Coloured Contours",     source => "cont_colours",           cat => "Basic",        lang => "c",    fordocs => "yes",  ext => "yes", description => "Coloured Contours"},
 		{ name => "Shaded Contours",       source => "cont_solidshade_test2",  cat => "Basic",        lang => "c",    fordocs => "yes",  ext => "yes", description => "Shaded Contours"},
 		{ name => "Array Data",            source => "cont_array",             cat => "Basic",        lang => "c",    fordocs => "yes",  ext => "yes", description => "Array Input"},
 		{ name => "Reference Tables",      source => "ref_tables",             cat => "Basic",        lang => "c",    fordocs => "yes",  ext => "yes", description => "Reference Tables"},
		) ;
		
	return @c_tests_menu;
}


sub c_testsuite_submenus
{
    my @c_ts_subs_menu = (
        { name => "Basic"      ,  first => "cont_colours"},
    );

    return @c_ts_subs_menu;
}


sub c_devices
{
    my @c_devices_list = ("ps");
    
    return @c_devices_list;
}


# ---------------------------------------------------------------------------------
# test_suite_selection_html
# Returns html code with a table for selecting the available test suites
# Parameter 0: the name of the current suite (so we don't link to it unnecessarily)
# ---------------------------------------------------------------------------------

sub test_suite_selection_html
{
    local($this_table) = @_[0];
    my $parent_dir     = ($this_table eq "Classic")  ? "../../test" : "..";
       $parent_dir     = ($this_table eq "Filename") ? "../../../test" : $parent_dir;
    my $classic_dir    = ($this_table eq "Classic") ? " "          : "../..";

    my $fortran_link        = ($this_table eq "Fortran")         ? "Fortran"  :        "<a href=\"$parent_dir/fortran/cont_noshade_ex_CYLINDRICAL.html\">Fortran</a>";
    my $fortran_static_link = ($this_table eq "Fortran_Static")  ? "Fortran_Static"  : "<a href=\"$parent_dir/fortran_static/cont_noshade_ex_CYLINDRICAL.html\">Fortran_Static</a>";
    my $classic_link        = ($this_table eq "Classic")         ? "Classic"  :        "<a href=\"$classic_dir/classic/fortran/cont01_CYLINDRICAL.html\">Classic</a>";
    my $c_link              = ($this_table eq "C")               ? "C"        :        "<a href=\"$parent_dir/c/cont_colours_CYLINDRICAL.html\">C</a>";
    my $magml_link          = ($this_table eq "MagML")           ? "MagML"    :        "<a href=\"$parent_dir/magml/contour_simple.html\">MagML</a>";
    my $epsgram_link        = ($this_table eq "EPSgram")         ? "EPSgram"  :        "<a href=\"$parent_dir/epsgram/eps-reading.html\">EPSgram</a>";
    my $filenamelink        = ($this_table eq "Filename")        ? "Filename" :        "<a href=\"$parent_dir/filenames/c/filenames.html\">Filenames</a>";
    my $small_start = "<font size=\"-2\">";
    my $small_end   = "</font>";

    my $selection_table =
"
    <table border = \"1\" bgcolor = \"#DDEEEE\">
    <tr>
        <td>$small_start<b>Test suite:</b>$small_end</td>
        <td>$small_start$fortran_link$small_end</td>
        <td>$small_start$fortran_static_link$small_end</td>
        <td>$small_start$classic_link$small_end</td>
        <td>$small_start$c_link$small_end</td>
        <td>$small_start$magml_link$small_end</td>
        <td>$small_start$epsgram_link$small_end</td>
        <td>$small_start$filenamelink$small_end</td>
    </tr>
    </table>
";
    
    return $selection_table;
}


1;  # don't forget to return a true value from the file  
