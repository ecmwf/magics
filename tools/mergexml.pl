# *****************************************************************************
# Perl script to automatically merge all the MAGICS++ objects' .xml files
# into one combined .xml file.
#
# Command-line options:
#   -debug=?  where ? = 0, 1, 2, or 3
#      This sets the level of debug information that is output.
#      (0 - none; 1 - important only; 2 - detailed, 3 - very detailed)
#
#   -XMLDir='dir' where dir is the directory in which the xml files reside.
#
# Remember to use the -s option when calling Perl - eg:
#   perl -s mergexml.pl -XMLDir='src/xml' -debug=1
#
# Description:
#   
# Authorship:
#   Iain Russell, ECMWF
#   Started:       7th July 2004
#   Last modified: 14th January 2005
# *****************************************************************************


# ---------------
# Useful globals
# ---------------

# $debug             = 2;  # set this to override the command-line option

$strDefaultXMLDir = 'src/xml';  # if no command-line option is specified
$strMergedFile    = "$XMLDir/magics.xml";
$strHeader        = "<?xml-stylesheet type='text/css' href='parameter.css'?>";
$strMagicsBegin   = "<magics>";
$strMagicsEnd     = "</magics>";


# if no user-supplied xml directory, then use the default.

if ($XMLDir eq '')
{
    $XMLDir = $strDefaultXMLDir;
}



# -------------------------------------------------------------------------
# Step 0 - determine the output files to create - and open them for writing
# -------------------------------------------------------------------------

open(MERGED_FILE, ">" . $strMergedFile);     # open for write, overwrite

print MERGED_FILE "$strHeader\n$strMagicsBegin";




# --------------------------------------------------------
# Step 1 - add each .xml file in the directory
# --------------------------------------------------------

# Can we read the directory?

if (opendir(DIR,"$XMLDir"))
{
	# Yes - examine each file in turn

	while ($file = readdir(DIR))
	{
		# only process .xml files, but not magics.xml

		if ( ($file ne "magics.xml") && ($file =~ /.*\.xml/) )
		{		
			append_xml ("$XMLDir/$file");
		}
	}
}

else
{
	debug_print("Directory $XMLDir cannot be read.\n", 1);
}


# ----------------
# Step 2 - cleanup
# ----------------

print MERGED_FILE "\n\n$strMagicsEnd\n";

closedir(DIR);




# ----------------------------------Subroutines ----------------------------------







# *****************************************************************
# Subroutine : append_xml
# Appends the given xml data to the merged file
# Parameter 1 ($_[0]) : the path to the .xml file to append
# *****************************************************************

sub append_xml
{
	# Read the input parameters

	local($file) = $_[0];

	debug_print ("appending $file\n", 2);
	
	print MERGED_FILE "\n\n";


	# Read the file and parse, line by line
	# OK, a Perl magician may be able to come up with a more
	# concise way of doing this, but this method will work.

	open (FILE, $file) || die "cannot open file $file";

	@Lines = <FILE>;

	foreach $line (@Lines)
	{
		# don't append the header line(s)
		
		if (($line !~ /<\?xml.*/) && ($line !~ /<magics.*/) && ($line !~ /<\/magics.*/))
		{
			print MERGED_FILE "$line";
		}
	}
	
	close(FILE);
}

# End of subroutine 'append_xml'






# ******************************************************************
# Subroutine : debug_print
# Prints the given string if debug is on
# Parameter 1 ($_[0]): text to print
# Parameter 2 ($_[1]): debug level to print it at
# ******************************************************************

sub debug_print
{
	local($text)  = @_[0];
	local($level) = @_[1];

	# Are we at a high enough debug level to print this text?

	if ($debug >= $level)
	{
		print ("$text");
	}
}

# End of subroutine 'debug_print'





