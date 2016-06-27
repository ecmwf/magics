#!/usr/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

use strict;
use Time::localtime;

my $projection = @ARGV[0];
my $class  = "${projection}Projection";
shift @ARGV;
my @parents = @ARGV;

my $license_file = '../LICENSE_for_source_files';	# Name the file
open(INFO, $license_file);				# Open the file
my @license = <INFO>;					# Read it into an array
close(INFO);						# Close the file

my $date_string=ctime();

package Function;

sub new { 
    my ($class, $name, $comment, $out, $const, @args) = @_;
    my $self = {};
    print "$#args\n";
  
    $self->{nb_args}= $#args;
    my $i = 0;
	foreach my $arg (@args)
	{		
		$self->{args}[$i] = $arg;
		$i++;
	}
	
   
    $self->{name}=$name;
    $self->{comment}=$comment;
    $self->{out}=$out;
    $self->{const}=$const;
   
    bless $self;
    return $self;
}

sub header  {
    my $self = shift;  
    
    print "\t/*!\n\t\\\\brief $self->{comment}\n\t*/\n";
    
    print "\tvirtual $self->{out} $self->{name}(";
    my $sep = "";
    if (! $self->{no_arg} ) {
    
    my $i = 0;   
    while ($i < $self->{nb_args} ) {
    	my @args = @{$self->{args}};
    	print "$sep@args[$i]";
		$sep=", ";
		$i+=2;
	}
    }
	print ") $self->{const};\n";
}
sub implementation  {
    my $self = shift;  
    print "$self->{out} $class\::$self->{name}(";
    my $sep = "";
    
    my $out = "";
	$out =  "return " unless    $self->{out} eq "void";
   
    my $i = 0;
    while ($i < $self->{nb_args}) {
    	my @args = @{$self->{args}};
		print "$sep@args[$i++] @args[$i++]";
		$sep = ", ";
	}
	print ")  $self->{const}\n{\n";
	print "\tMagLog::dev() << \"$class\::$self->{name}(...) needs implementing.\" << endl;\n";
	print "\t$out\Transformation::$self->{name}(";
	my $sep = "";
    my $i = 0;
    while ($i < $self->{nb_args}) {
    	$i++;
    	my @args = @{$self->{args}};
		print "$sep@args[$i++]";
		$sep = ", ";
	}
	print ");\n}\n\n";
}

my @functions = ( 
    Function->new("init", 
    	"Initialise the projection", 
    	"void", ""),
    Function->new("operator()", 
    	"", 
    	"PaperPoint", "const", 
    	"const UserPoint&", "point"),
	Function->new("operator()", 
		"", 
		"PaperPoint", "const", 
		"const GeoPoint&", "point"),
	Function->new("operator()", 
		"", 
		"PaperPoint", "const", 
		"const PaperPoint&", "point"),
	Function->new("revert", 
		"", 
		"void", "const", 
		"const PaperPoint&", "xy", 
		"GeoPoint&", "point"),
	Function->new("revert", 
		"", 
		"void", "const", 
		"const PaperPoint&", "xy", 
		"UserPoint&", "point"),
	Function->new("needShiftedCoastlines", 
		"Does the projection needs the coastalines to be shifted!", 
		"bool", "const" ),
	Function->new("aspectRatio", 
 		"set the aspect ratio!", 
 		"void", 
 		"", 
 		"double&", "width",
 		"double&", "height"),
 	Function->new("boundingBox", 
 		"set the bounding box!", 
 		"void", 
 		"const", 
 		"double&", "xmin",
 		"double&", "ymin",
 		"double&", "xmax",
 		"double&", "ymax"),
 	Function->new("getMinX", 
 		"return the xmin in user coordinates!", 
 		"double", 
 		"const"),
 	Function->new("getMinY", 
 		"return the ymin in user coordinates!", 
 		"double", 
 		"const"),
   Function->new("getMaxX", 
 		"return the xmax in user coordinates!", 
 		"double", 
 		"const"),
 		Function->new("getMaxY", 
 		"return the ymax in user coordinates!", 
 		"double", 
 		"const"),
 		Function->new("setMinX", 
 		"set the xmin in user coordinates!", 
 		"void", 
 		"",
 		"double", "x"),
 	Function->new("setMinY", 
 		"return the ymin in user coordinates!", 
 		"void", 
 		"",
 		"double", "y"),
   Function->new("setMaxX", 
 		"return the xmax in user coordinates!", 
 		"void", 
 		"",
 		"double", "x"),
 		Function->new("setMaxY", 
 		"return the ymax in user coordinates!", 
 		"void", 
 		"",
 		"double", "y"),
 		Function->new("getMinPCX", 
 		"return the xmin in projection coordinates!", 
 		"double", 
 		"const"),
 	Function->new("getMinPCY", 
 		"return the ymin in projection coordinates!", 
 		"double", 
 		"const"),
   Function->new("getMaxPCX", 
 		"return the xmax in projection coordinates!", 
 		"double", 
 		"const"),
 		Function->new("getMaxPCY", 
 		"return the ymax in projection coordinates!", 
 		"double", 
 		"const"),
 	 Function->new("gridLongitudes", 
 		"create the grid for the longitudes!!", 
 		"void", 
 		"const",
 		"const GridPlotting&", "grid",
 		"GraphicsList&", "out"),
     Function->new("gridLatitudes", 
 		"create the grid for the latitudes!!", 
 		"void", 
 		"const",
 		"const GridPlotting&", "grid",
 		"GraphicsList&", "out"),
 	  Function->new("topLabels", 
 		"calculate the top labels", 
 		"void", 
 		"const",
 		"const BasicSceneObject&", "object",
 		"const LabelPlotting&", "label",
 		"GraphicsList&", "out"),
 	Function->new("bottomLabels", 
 		"calculate the top labels", 
 		"void", 
 		"const",
 		"const BasicSceneObject&", "object",
 		"const LabelPlotting&", "label",
 		"GraphicsList&", "out"),
 	Function->new("leftLabels", 
 		"calculate the left labels", 
 		"void", 
 		"const",
 		"const BasicSceneObject&", "object",
 		"const LabelPlotting&", "label",
 		"GraphicsList&", "out"),
 	Function->new("rightLabels", 
 		"calculate the right labels", 
 		"void", 
 		"const",
 		"const BasicSceneObject&", "object",
 		"const LabelPlotting&", "label",
 		"GraphicsList&", "out"),
 	Function->new("thin", 
 		"thin the position for wind plotting", 
 		"void", 
 		"const",
 		"MatrixHandler<GeoPoint>&", "data",  
 		"double", "x",
 		"double", "y",
 		"vector<GeoPoint>&", "out"),	
 	Function->new("javascript",       
 		"prepare the javascript for thes particular instance of projection", 
 		"string", "",
        "double", "x",
 		"double", "y", 
        "double", "width", 
        "double", "height"),	
 ); 
 
 
 
 
 foreach my $function  (@functions) {
 	print ("hello");
       $function->header();
}
	


########################################
# Header file
########################################
open STDOUT, ">$class.h";
print <<"EOF";
/******************************** LICENSE ********************************

@license

 ******************************** LICENSE ********************************/

/*!
    \\file $class.h
    \\brief Definition of $class.
    \\author Graphics Section, ECMWF

    Started: $date_string
*/

#ifndef \_$class\_H
#define \_$class\_H

#include <Transformation.h>
#include <${class}Attributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \\class $class
    \\brief Implements a new projection
    \\ingroup projection

    This projection ...
*/

class ${class}: public Transformation, public ${class}Attributes
{

public:
	$class();
	~$class();

	/*!
	  \\brief sets  from an XML node
	*/
	void set(const XmlNode& node)
	{
        Transformation::set(node);
        ${class}Attributes::set(node);
	}
   /*!
	  \\brief sets  from a map
	*/
	void set(const map<string, string>& map)
	{
        Transformation::set(map);
        ${class}Attributes::set(map);
	}
    
    virtual Transformation* clone() const {
		${class}* transformation = new ${class}();
        transformation->copy(*this);
		return transformation;
	}
	
EOF

foreach my $function  (@functions) {
       $function->header();
}
	
print <<"EOF";
	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	$class(const $class&);
    //! Overloaded << operator to copy - No copy allowed
	$class& operator=(const $class&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const $class& p)
		{ p.print(s); return s; }

};
    

} // namespace magics
#endif
EOF
close STDOUT;



open STDOUT, "> $class\.cc";

print<<"EOF";
/******************************** LICENSE ********************************

@license

 ******************************** LICENSE ********************************/

/*! \\file $class\.cc
    \\brief Implementation of $class.
    \\author Graphics Section, ECMWF

    Started: $date_string

*/

#include <$class\.h>

using namespace magics;

/*!
  \\brief Constructor
*/
$class\::$class() 
{
	MagLog::debug() << "$class\::$class needs implementing." << std::endl;
}

/*!
  \\brief Destructor
*/
$class\::~$class() 
{
}

void $class\::print(ostream& out) const
{
    out << "$class\[";
    $class\Attributes::print(out);
    out << "]"; 
} 

EOF

foreach my $function  (@functions) {
       $function->implementation();
}
	
print <<"EOF";


EOF

close STDOUT;

##############################################
# XML attribute file
##############################################
open STDOUT, ">$class.xml";
print <<"EOF";
<?xml-stylesheet type='text/css' href='parameter.css'?>
<!--
 ******************************** LICENSE ********************************
 
@license

 ******************************** LICENSE ********************************
-->
<magics>
<class name='$class' action='projection' directory='common'>
	<documentation>
		These are the attributes of the $projection projection. 
	</documentation>
<!--
	<parameter name='' from='string' to='string' member='' default='' xml=''>
            <documentation></documentation>
        </parameter>
-->
</class>
</magics>

EOF

close STDOUT;

