/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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
#include <TeVisual.h>

TeBaseVisualParams::TeBaseVisualParams(TeGeomRep rep, const string& visualType) :
		symbId_ (TePolyTypeFill),
		color_ (255,0,0),
	    transparency_ (0),
		width_ (0),
	   	contourSymbId_ (TeLnTypeContinuous),
		contourColor_(0,255,255),
		contourTransparency_ (0),
		contourWidth_ (1),
		sizeValue_ (0),
		ptAngle_ (0),
		family_ ("Verdana"),
		bold_ (false),
		italic_(false),
		alignmentVert_(0),
		alignmentHoriz_(0),
		tabSize_(6),
		lineSpace_(1),
		fixedSize_(false),
		visualType_(visualType),
		geomRep_(rep)
{
	switch (rep)
	{
		case TePOLYGONS:
		case TeCELLS:
			symbId_= TePolyTypeFill;
			contourSymbId_= TeLnTypeContinuous;
			break;
		case TeLINES:
			symbId_= TeLnTypeContinuous;
			width_ = 1;
			break;
		case TePOINTS:
		case TeNODES:
			symbId_= TePtTypeBox;
			sizeValue_ = 3;
			break;
		case TeTEXT:
			sizeValue_= 12;
			break;
		default:
			break;
	}
}

// Copy constructor
TeBaseVisualParams::TeBaseVisualParams(const TeBaseVisualParams& other)
{	
	symbId_ = other.symbId_;
	color_ = other.color_;
	transparency_ = other.transparency_;
	width_ = other.width_;

	contourSymbId_ = other.contourSymbId_;
	contourColor_ = other.contourColor_;
	contourTransparency_ = other.contourTransparency_;
	contourWidth_ = other.contourWidth_;
		
	sizeValue_ = other.sizeValue_;
	ptAngle_ = other.ptAngle_;
	
	family_ = other.family_;
	bold_ = other.bold_;
	italic_ = other.italic_;
	fixedSize_ = other.fixedSize_;
	alignmentVert_ = other.alignmentVert_;
	alignmentHoriz_ = other.alignmentHoriz_;
	tabSize_ = other.tabSize_;
	lineSpace_ = other.lineSpace_;

	visualType_ = other.visualType_;
	geomRep_ = other.geomRep_; 
}

// Assignment operator
TeBaseVisualParams& 
TeBaseVisualParams::operator=(const TeBaseVisualParams& vis)
{
	if ( this != &vis )
	{
		symbId_ = vis.symbId_;
		color_ = vis.color_;
		transparency_ = vis.transparency_;
		width_ = vis.width_;

		contourSymbId_ = vis.contourSymbId_;
		contourColor_ = vis.contourColor_;
		contourTransparency_ = vis.contourTransparency_;
		contourWidth_ = vis.contourWidth_;
		
		sizeValue_ = vis.sizeValue_;
		ptAngle_ = vis.ptAngle_;
		
		family_ = vis.family_;
		bold_ = vis.bold_;
		italic_ = vis.italic_;
		fixedSize_ = vis.fixedSize_;
		alignmentVert_ = vis.alignmentVert_;
		alignmentHoriz_ = vis.alignmentHoriz_;
		tabSize_ = vis.tabSize_;
		lineSpace_ = vis.lineSpace_;

		visualType_ = vis.visualType_;
		geomRep_ = vis.geomRep_;
	}
	return *this;
}

// Returns TRUE if param1 is equal to param2 or FALSE if they are different.
bool 
TeBaseVisualParams::operator== (const TeBaseVisualParams& vis)
{
	return ( symbId_ == vis.symbId_ &&
		color_ == vis.color_ &&
		transparency_ == vis.transparency_ &&
		width_ == vis.width_ &&
		contourSymbId_ == vis.contourSymbId_ &&
		contourColor_ == vis.contourColor_ &&
		contourTransparency_ == vis.contourTransparency_ &&
		contourWidth_ == vis.contourWidth_ &&
		sizeValue_ == vis.sizeValue_ &&
		ptAngle_ == vis.ptAngle_ &&
		family_ == vis.family_ &&
		bold_ == vis.bold_ &&
		italic_ == vis.italic_ &&
		fixedSize_ == vis.fixedSize_ &&
		alignmentVert_ == vis.alignmentVert_ &&
		alignmentHoriz_ == vis.alignmentHoriz_ &&
		tabSize_ == vis.tabSize_ && 
		lineSpace_ == vis.lineSpace_  &&
		visualType_ == vis.visualType_ &&
		geomRep_ == vis.geomRep_);
}

// Returns visual to the default parameters 
void 
TeBaseVisualParams::clear()
{
	symbId_ = TePolyTypeFill;
	color_ = TeColor(255,0,0);
	transparency_=0;
	width_ = 0; 
	contourSymbId_ = TeLnTypeContinuous;
	contourColor_ = TeColor (0,255,255);
	contourTransparency_ = 0;
	contourWidth_ = 1;
	sizeValue_ = 0;
	ptAngle_ = 0;
	family_ = "Verdana";
	bold_  = false;
	italic_ = false;
	alignmentVert_ = 0;
	alignmentHoriz_ = 0;
	tabSize_ = 6;
	lineSpace_ = 1;
	fixedSize_ = false;
	geomRep_ = TePOLYGONS;
	visualType_="tevisual";
}

// Copies only the basic visual parameters from another visual
void 
TeBaseVisualParams::setBasic (TeBaseVisualParams &vis)
{
	symbId_ = vis.symbId_;
	color_ = vis.color_;
	transparency_ = vis.transparency_;
	width_ = vis.width_;

	contourSymbId_ = vis.contourSymbId_;
	contourColor_ = vis.contourColor_;
	contourTransparency_ = vis.contourTransparency_;
	contourWidth_ = vis.contourWidth_;
	
	sizeValue_ = vis.sizeValue_;
	ptAngle_ = vis.ptAngle_;
	
	family_ = vis.family_;
	bold_ = vis.bold_;
	italic_ = vis.italic_;
	fixedSize_ = vis.fixedSize_;
	alignmentVert_ = vis.alignmentVert_;
	alignmentHoriz_ = vis.alignmentHoriz_;
	tabSize_ = vis.tabSize_;
	lineSpace_ = vis.lineSpace_;
	geomRep_ = vis.geomRep_;
	visualType_="tevisual";
}

//--------------

TeVisual::TeVisual(TeGeomRep rep) : 
		params_ (TeBaseVisualParams(rep, "tevisual"))
{
}
		
// Copy constructor
TeVisual::TeVisual(const TeVisual& other)
{	
	params_ = other.params_;
}

// Assignment operator
TeVisual& 
TeVisual::operator=(const TeVisual& vis)
{
	if ( this != &vis )
		params_ = vis.params_;
	return *this;
}

// Returns TRUE if param1 is equal to param2 or FALSE if they are different.
bool 
TeVisual::operator== (const TeVisual& vis)
{
	return ( params_ == vis.params_);
}

// Returns visual to the default parameters 
void 
TeVisual::clear()
{
	params_.clear();
}

// Copies only the basic visual parameters from another visual
void 
TeVisual::setBasic (TeVisual &vis)
{
	params_.setBasic(vis.params_);
}


