/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeVisual.h
    \brief This file deals with the visual presentation characteristics of geometries in TerraLib
*/
#ifndef  __TERRALIB_INTERNAL_VISUAL_H
#define  __TERRALIB_INTERNAL_VISUAL_H

#include "TeDataTypes.h"
#include "TeAbstractFactory.h"

class TeRasterTransform;

#include <string>
#include <map>

using namespace std;


//! A structure for supporting a color definition
struct TL_DLL TeColor
{
	//! Red component
	int red_;

	//! Green component 
	int green_; 	
	
	//! Blue component
	int blue_;	
	
	//! Color name
	string name_;
	
	//! Empty constructor
	TeColor () : red_(0),green_(0),blue_(0), name_("") {}

	//! Constructor with parameters
	TeColor (int r, int g, int b, const string& name="") : red_(r),green_(g),blue_(b), name_(name) {}

	//! Set parameters of colors
	void init (int r, int g, int b, const string& name="") {red_=r,green_=g,blue_=b;name_=name; }


	//! Returns TRUE if color1 is equal to color2 or FALSE if they are different.
	bool operator== (const TeColor& color)
	{
		return (red_==color.red_ && green_==color.green_ && blue_==color.blue_);
	}

	//! Assignment operator
	TeColor& operator= ( const TeColor& color )
	{
		if ( this != &color )
		{	
			red_ = color.red_;
			green_ = color.green_;
			blue_ = color.blue_;
			name_ = color.name_;
		}
		return *this;
	}
};

//!  A class to represent the base visual parameters 
/*!  
	 This class contains the base visual parameters and it is used 
	 by the factory responsible for creating visual objects. 
	 
	\sa
     TeVisual
*/
class TL_DLL TeBaseVisualParams
{
public:
	//! Constructor
	TeBaseVisualParams(TeGeomRep rep = TePOLYGONS, const string& visualType = "tevisual"); 

	//! Copy constructor
	TeBaseVisualParams(const TeBaseVisualParams& other);

	//! Destructor
	virtual ~TeBaseVisualParams() {}

	//! Assignment operator
	virtual TeBaseVisualParams& operator=(const TeBaseVisualParams& visParams);

	//! Returns TRUE if param1 is equal to param2 or FALSE if they are different.
	virtual bool operator== (const TeBaseVisualParams& visParams);

	//! Returns visual to the default parameters 
	virtual void clear();

	//! Copies only the basic visual parameters from another visual
	virtual void setBasic (TeBaseVisualParams &visParams);

	//! Returns the visual params type 
	string getProductId() { return visualType_; }

	//! Symbol id that define the style for the geometry filling
    int			symbId_;			
	//! Color used for geometry filling (line, point and text color)
	TeColor		color_;				
	//! Percentage of transparency for the geometry filling
	int			transparency_;		
	//! Width for line geometry
	int			width_;				
	
	//! Symbol id that define the style for the geometry outline
	int			contourSymbId_;			
	//! Contour color for the geometry outline
	TeColor		contourColor_;			
	//! Percentage of transparency for the geometry outline
	int			contourTransparency_;	
	//! Width for the geometry outline
	int			contourWidth_;			

	//! Point size
	int			sizeValue_;	
	//! Point angle
	int			ptAngle_;				
	
	// Text attributes
	//! Text family
	string		family_;	
	//! Flag to indicate if text is bold
	bool		bold_;			
	//! Flag to indicate if text is italic
	bool		italic_;	
	//! Position of vertical alignment
	double		alignmentVert_;		
	//! Position of horizontal alignment
	double		alignmentHoriz_;	
	//! Number of spaces defined in a tab character
	int			tabSize_;		
	//! Spacing between lines
	int			lineSpace_;	
	//! Flag to indicate that element has a fixed size, dont follow zoom operations	
	bool		fixedSize_;			

	//! Visual type used in factory strategy
	string		visualType_;
	//! Geometric representation associated with this visual 
	TeGeomRep   geomRep_;				
};

//! This class is used to store the standard presentation characteristics of vector geometries
class TL_DLL TeVisual
{
public :
	
	//! Empty constructor
	TeVisual(TeGeomRep rep = TePOLYGONS);

	//! Destructor
	virtual ~TeVisual() {}

	//! Copy constructor
	TeVisual(const TeVisual& other);

	//! Assignment operator
	virtual TeVisual& operator=(const TeVisual& vis);

	//! Returns TRUE if param1 is equal to param2 or FALSE if they are different.
	virtual bool operator== (const TeVisual& vis);

	//! Returns visual to the default parameters 
	virtual void clear();

	//! Copies only the basic visual parameters from another visual
	virtual void setBasic (TeVisual &vis);

	//! Sets the color used for geometry filling and line, point and text color
	virtual void color(TeColor& color) 
	{	params_.color_ = color;}

	//! Returns the color used for geometry filling and line, point and text color
	virtual TeColor& color() 
	{ return params_.color_; }

	//! Sets the style for the filling of geometries
	virtual void style(int s) 
	{ params_.symbId_ = s; }

	//! Returns the style for the filling of geometries
	virtual int	style() 
	{ return params_.symbId_; }

	//! Sets the percentage of transparency for polygons and cells
	virtual void transparency(int t)
	{ params_.transparency_ = t; }

	//! Returns the percentage of transparency used in polygons and cells
	virtual int transparency()
	{ return params_.transparency_; }

	//! Sets the color for the polygons and cells outlines
	virtual void contourColor(TeColor& color) 
	{ params_.contourColor_ = color;}

	//! Returns the color used in the polygons and cells outlines
	virtual TeColor& contourColor() 
	{ return params_.contourColor_; }

	//! Sets the percentage of transparency for the polygon ans cells outlines
	virtual void contourTransparency(int t) 
	{ params_.contourTransparency_ = t;}

	//! Returns the color used in the polygons and cells outlines
	virtual int	contourTransparency() 
	{ return params_.contourTransparency_; }

	//! Sets the contour width for the polygons and cells outlines
	virtual void contourWidth(int w) 
	{ params_.contourWidth_ = w; }

	//! Returns the contour width used in the polygons and cells outlines
	virtual int	contourWidth()  
	{ return params_.contourWidth_; }

	//! Sets the contour style for the polygons and cells outlines
	virtual void contourStyle(int s) 
	{ params_.contourSymbId_ = s; }

	//! Returns the contour style used in the polygons and cells outlines
	virtual int	contourStyle() 
	{ return params_.contourSymbId_; }

	//! Sets the width
	virtual void width(int w) 
	{ params_.width_ = w; }

	//! Returns the width
	virtual int	 width ()  
	{ return params_.width_; }

	//! Sets the point size
	virtual void size (int s)  
	{ params_.sizeValue_ = s; }

	//! Returns the point size
	virtual int	size ()  
	{ return params_.sizeValue_; }

	//! Sets the text family for fonts
	virtual void family (string f) 
	{ params_.family_ = f; }

	//! Returns fonts family
	virtual string family () 
	{ return params_.family_ ; }

	//! Sets the bold style for texts
	virtual void bold (bool b) 
	{ params_.bold_ = b; }

	//! Returns the bold style used in the texts
	virtual bool bold () 
	{ return params_.bold_; }

	//! Sets the italic style for texts
	virtual void italic (bool i) 
	{ params_.italic_ = i; }

	//! Returns the italic style used in the texts
	virtual bool italic () 
	{ return params_.italic_; }

	//!  Sets a flag indicating if the text will have a fixed size
	virtual void fixedSize (bool i) 
	{ params_.fixedSize_ = i; }

	//!  Retuns a flag indicating if an text have a fixed size
	virtual bool fixedSize () 
	{ return params_.fixedSize_; }

	//!  Sets the vertical alignment of texts
	virtual void alignmentVert(double a) 
	{ params_.alignmentVert_ = a; }

	//!  Retuns the vertical alignment of texts
	virtual double alignmentVert() 
	{ return params_.alignmentVert_; }

	//!  Sets the horizontal alignment of texts
	virtual void alignmentHoriz(double a) 
	{ params_.alignmentHoriz_ = a; }

	//!  Retuns the horizontal alignment of texts
	virtual double alignmentHoriz() 
	{ return params_.alignmentHoriz_; }

	//! Sets the number of spaces defined in a tab character
	virtual void tabSize (int s) 
	{ params_.tabSize_ = s; }

	//! Returns the number of spaces defined in a tab character
	virtual int	tabSize () 
	{ return params_.tabSize_; }

	//! Sets the spacing between lines
	virtual void lineSpace (int s) 
	{ params_.lineSpace_ = s; }

	//! Returns the spacing between lines
	virtual int	lineSpace () 
	{ return params_.lineSpace_; }

	//! Returns the angle of points and symbols
	virtual int   ptAngle() 
	{return params_.ptAngle_; }

	//! Sets the angle of points and symbols
	virtual void  ptAngle(int p) 
	{params_.ptAngle_ = p;}

	//! Creates another visual pointer and copies its content
	virtual TeVisual* copy()
	{
		TeVisual* retval = new TeVisual();
		*retval = *this;
		return retval;
	}

	//! Returns a default visual object
	static TeVisual* DefaultObject()  
	{ return new TeVisual(); }

protected:
	TeBaseVisualParams	params_;
};


//! A factory to build TeVisual objects
class TL_DLL TeVisualFactory : public TeAbstractFactory<TeVisual, TeBaseVisualParams, string>
{
public:
	TeVisualFactory(const string& facName) : TeAbstractFactory<TeVisual, TeBaseVisualParams, string>(facName)
	{}
		virtual TeVisual* build ()
	{	return new TeVisual(); }
};

//! Export the type that represents the definition of visual for raster data
typedef TeRasterTransform TeRasterVisual;

#endif

