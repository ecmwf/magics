#!/usr/bin/env perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

use strict;
use Time::localtime;

my $driver = @ARGV[0];
my $class  = "${driver}Driver";
shift @ARGV;
my @parents = @ARGV;

my $license_file = '../LICENSE_for_source_files';	# Name the file
open(INFO, $license_file);				# Open the file
my @license = <INFO>;					# Read it into an array
close(INFO);						# Close the file

my $date_string=ctime();

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
    \\author Meteorological Visualisation Section, ECMWF

    Started: $date_string
*/

#ifndef \_MPP\_$class\_H
#define \_MPP\_$class\_H

#include <BaseDriver.h>
#include <${class}Attributes.h>
#include <XmlNode.h>

namespace magics
{

/*! \\class $class
    \\brief This driver produces output for $driver
    \\ingroup drivers

    This driver ...
*/
class ${class}: public BaseDriver, public ${class}Attributes
{

public:
	$class();
	~$class();
	void open();
	void close();

	/*!
	  \\brief sets a new XML node
	*/
	void set(const XmlNode& node)
	{
/* >>> adopt to your needs
		if ( magCompare(node.name(), "ps") ||
		     magCompare(node.name(), "eps") ||
		     magCompare(node.name(), "pdf") )
		{
			XmlNode basic = node;
			basic.name("driver");
			BaseDriver::set(basic);
			basic.name("ps");
			${class}Attributes::set(basic);
		}
  <<< adopt to your needs */
	}

	/*!
	  \\brief sets a new map
	*/
	void set(const map<std::string, std::string>& map)
	{
		BaseDriver::set(map);
		${class}Attributes::set(map);
	}

private:
	MAGICS_NO_EXPORT void startPage() const;
	MAGICS_NO_EXPORT void endPage() const;
	MAGICS_NO_EXPORT void project(const Layout& lay) const;
	MAGICS_NO_EXPORT void unproject() const;

	MAGICS_NO_EXPORT void setNewLineWidth(const float) const;
	MAGICS_NO_EXPORT void setNewColour(const Colour &col) const;
	MAGICS_NO_EXPORT int setLineParameters(const LineStyle style, const float w) const;

	MAGICS_NO_EXPORT void renderPolyline(const int, float *, float *) const;
	MAGICS_NO_EXPORT void renderPolyline2(const int n, float *x, float *y) const;
	MAGICS_NO_EXPORT void renderSimplePolygon(const int, float *, float *) const;
	MAGICS_NO_EXPORT void renderText(const Text& text) const;
	MAGICS_NO_EXPORT void circle(const float x, const float y, const float r, const int) const;
	MAGICS_NO_EXPORT bool renderPixmap(float,float,float,float,int,int,unsigned char*,int,bool) const;
	MAGICS_NO_EXPORT bool renderCellArray(const Image& obj) const;

	// $class specific member functions BEGIN

	// $class specific member functions END

	//! Method to print string about this class on to a stream of type ostream (virtual).
	void print(ostream&) const;
	MAGICS_NO_EXPORT void debugOutput(const string &s) const;

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

open STDOUT, ">$class.cc";
print <<"EOF";
/******************************** LICENSE ********************************

@license

 ******************************** LICENSE ********************************/

/*! \\file $class.cc
    \\brief Implementation of $class.
    \\author Meteorological Visualisation Section, ECMWF

    Started: $date_string

*/

#include <$class\.h>
#include <Polyline.h>
#include <Text.h>
#include <Image.h>

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

/*!
  \\brief Opening the driver
*/
void $class\::open()
{
	MagLog::debug() << "$class\::open() needs implementing." << std::endl;
}

/*!
  \\brief Closing the driver
*/
void $class\::close()
{
	endPage();
	currentPage_ = 0;

	MagLog::debug() << "$class\::close() needs implementing." << std::endl;
}

/*!
  \\brief starting a new page

  This method has to take care that previous pages are closed and that
  for formats with multiple output files a new file is set up.
*/
MAGICS_NO_EXPORT void $class\::startPage() const
{
	if(currentPage_ > 0) endPage();

	MagLog::debug() << "$class\::startPage needs implementing." << std::endl;
	
	currentPage_++;
	newPage_ = true;
}

/*!
  \\brief ending a page
 
  This method has to take care that for formats with multiple output 
  files are closed.
*/
MAGICS_NO_EXPORT void $class\::endPage() const
{
	MagLog::debug() << "$class\::endPage needs implementing." << std::endl;
}

/*!
  \\brief project to a new Layout

  This method will update the offset and scale according to the new Layout given.

  \\sa Layout
*/
MAGICS_NO_EXPORT void $class\::project(const magics::Layout& layout) const
{
	MagLog::debug() << "$class\::project needs implementing." << std::endl;
}

/*!
  \\brief reproject out of the last Layout

  This method will update the offset and scale to the state they were before the
  last Layout was received.

*/
MAGICS_NO_EXPORT void $class\::unproject() const
{
	MagLog::debug() << "$class\::unproject needs implementing." << std::endl;
}


/*!
  \\brief sets a new colour

  This colour stays the default drawing colour until the painting in the 
  current box is finished.

  \\sa Colour
*/
MAGICS_NO_EXPORT void $class\::setNewColour(const Colour &colour) const
{
	if(currentColour_ == colour) return;
	currentColour_ = colour;
	MagLog::debug() << "$class\::setNewColour needs checking." << std::endl;
}

/*!
  \\brief sets a new line width

  This line width stays the default width until the painting in the 
  current box is finished.

  \\sa setLineParameters()
*/
MAGICS_NO_EXPORT void $class\::setNewLineWidth(const float width) const
{
	currentLineWidth_ = width;
	MagLog::debug() << "$class\::setNewColour needs checking." << std::endl;
}

/*!
  \\brief sets new properties of how lines are drawn

  These properties stay the default until the painting in the 
  current box is finished.

  \\sa LineStyle

  \\param linestyle Object describing the line style
  \\param w width of the line

*/
MAGICS_NO_EXPORT int $class\::setLineParameters(const LineStyle linestyle, const float w) const
{
	setNewLineWidth(w);

	MagLog::debug() << "$class\::setLineParameters needs implementing." << std::endl;
	return 0;
}

/*!
  \\brief renders polylines

  This method renders a polyline given as two float arrays. The two 
  arrays given as X and Y values have to be at least the length of
  <i>n</i>. All values beyond <i>n</i> will be ignored. The style is
  determined by what is described in the current LineStyle.

  \\sa setLineParameters()
  \\param n number of points
  \\param x array of x values
  \\param y array of y values
*/
MAGICS_NO_EXPORT void $class\::renderPolyline(const int n, float *x, float *y) const
{
	MagLog::debug() << "$class\::renderPolyline needs implementing." << std::endl;
}

/*!
  \\brief renders a single line

  This method renders a polyline with two points.The style is
  determined by what is described in the current LineStyle.

  \\sa setLineParameters()
  \\param n number of points
  \\param x array of x values
  \\param y array of y values
*/
MAGICS_NO_EXPORT void $class\::renderPolyline2(const int n, float* x, float* y) const
{
	MagLog::debug() << "$class\::renderPolyline2 needs implementing." << std::endl;
}

/*!
  \\brief renders a filled polygon

  This method renders a filled polygon. The style is
  determined by what is described in the current LineStyle.

  \\sa setLineParameters()
  \\param n number of points
  \\param x array of x values
  \\param y array of y values
*/
MAGICS_NO_EXPORT void $class\::renderSimplePolygon(const int n, float* x, float* y) const
{
	MagLog::debug() << "$class\::renderSimplePolygon needs implementing." << std::endl;
}

/*!
  \\brief renders text strings

  This method renders given text strings.

  \\note As of version 2.0 there are two forms of describing text in Text.

  \\sa Text
  \\param text object containing the strings and their description
*/
MAGICS_NO_EXPORT void $class\::renderText(const Text& text) const
{
	if(text.empty()) return;
	MagLog::debug() << "$class\::renderText needs implementing." << std::endl;
}

/*!
  \\brief drawing a circle

  This method renders given text strings.

  The meaning of the last parameter <i>s</i> is as follows:
     - 0-8 determines how many quarters of the circle are filled. Starting from the top clock-wise.
     - 9 fills the whole circle but leaves a vertical bar empty in the middle of the circle.

  \\param x X Position
  \\param y Y Position
  \\param r Radius of circle
  \\param s Style which determines how the circle is shaded
*/
MAGICS_NO_EXPORT void $class\::circle(const float x, const float y, const float r, const int s) const
{
	MagLog::debug() << "$class\::circle needs implementing." << std::endl;
}

/*!
  \\brief render pixmaps

  This method renders pixmaps. These are used for cell shading and raster input (GIFs and PNGs).

  \\sa renderCellArray()

  \\param x0 x of lower corner
  \\param y0 y of lower corner
  \\param x1 x of higher corner
  \\param y1 y of higher corner
  \\param w width of pixmap
  \\param h height of pixmap
  \\param pixmap contents
  \\param landscape says if contents is landscape

*/
MAGICS_NO_EXPORT bool $class\::renderPixmap(float x0,float y0,float x1,float y1,
                                            int w,int h,unsigned char* pixmap,int landscape, bool) const
{
	MagLog::debug() << "$class\::renderPixmap needs implementing." << std::endl;
	return true;
}

/*!
  \\brief render cell arrays

  This method renders cell arrays, also called images in Magics language. These are 
  mainly used for satellite data.

  \\sa renderPixmap()

  \\param image Object containing an image
*/
MAGICS_NO_EXPORT bool $class\::renderCellArray(const Image& image) const
{
	MagLog::debug() << "$class\::renderCellArray needs implementing." << std::endl;
	return true;
}



/*!
  \\brief prints debug output

  When Magics++ is compiled in debug mode these extra strings are printed.

  \\note This can increase file and MagLog file sizes if you run Magics++ in debug mode!

  \\param s string to be printed
*/
MAGICS_NO_EXPORT void $class\::debugOutput(const std::string &s) const
{
	MagLog::debug() << s << endl;
}

/*!
  \\brief class information are given to the output-stream
*/
void $class\::print(ostream& out)  const
{
	out << "$class\[";
	out << "]";
}

static SimpleObjectMaker<${class}, BaseDriver> ${driver}_driver("${driver}");

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
<class name='$class' action='$driver' directory='drivers' inherits='BaseDriver'>
	<documentation>
		These are the attributes of the $driver output driver. This driver is still in the developing stage! 
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

##############################################
# Further action file !!!
##############################################
open STDOUT, ">$class.todo";
print <<"EOF";



src/common/OutputHandler.cc
===========================

For each format supported by your driver add

#ifdef MAGICS_$driver
static SimpleObjectMaker<${driver}OutputFactory, OutputFactory> format("format");
#endif

src/common/OutputFactory.h
==========================

For each format supported by your driver add

#ifdef MAGICS_${driver}
class ${driver}OutputFactory : public OutputFactory
{
public:
	${driver}OutputFactory() {}
	virtual ~${driver}OutputFactory() {}
	
	virtual OutputFactory* clone() const
	{
		return new ${driver}OutputFactory();
	}
	virtual void set(DriverManager&, const XmlNode&); 
};
#endif

src/common/OutputFactory.cc
===========================

#ifdef MAGICS_${driver}
#include "${class}.h"
void ${driver}OutputFactory::set(DriverManager& magics, const XmlNode& node)
{
	
	${class}* driver = new ${class}();
	driver->set(node);
	magics.push_back(driver);
}
#endif


CONFIG & MAKEFILES:
===================

./src/drivers/Makefile.am  -  add driver (possibly with conditional)

./configure.ac  -  If you want a conditional you need to add this code

EOF

close STDOUT;
