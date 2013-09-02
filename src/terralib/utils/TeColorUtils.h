/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeColorUtils.h
    \brief This file contains functions to manipulate a structure representing a color
*/
#ifndef  __TERRALIB_INTERNAL_COLORUTILS_H
#define  __TERRALIB_INTERNAL_COLORUTILS_H


#include "TeVisual.h"
#include "TeUtils.h"

void rgb2Hsv(const TeColor& c, int& h, int& s, int& v);
void RGBtoHSV(const double& r, const double& g, const double& b, double& h, double& s, double& v );
void hsv2Rgb(TeColor& c, const int& h, const int& s, const int& v);
void HSVtoRGB( double& r, double& g, double& b, const double& h, const double& s, const double& v );

struct ColorBar {
	TeColor cor_;
	int		h_;
	int		s_;
	int		v_;
	double	distance_;

	void color(const TeColor& c){cor_ = c; rgb2Hsv(cor_, h_, s_, v_);}

	ColorBar& operator= (const ColorBar& cb)
	{
		cor_ = cb.cor_;
		h_ = cb.h_;
		s_ = cb.s_;
		v_ = cb.v_;
		distance_ = cb.distance_;

		return *this;
	}

	bool operator<= (const ColorBar& cb) const
	{
		return (distance_ <= cb.distance_);
	}

	bool operator< (const ColorBar& cb) const
	{
		return (distance_ < cb.distance_);
	}
};

#include <vector>
#include <string>
#include <map>

//! Generates a graduated color scale following a sequence of basic colors
/*!
	The possible basic colors are "RED", "GREEN", "BLUE", "YELLOW", "CYAN", "MAGENTA", "GRAY" and  "BLACK"
	\param ramps	vector with the sequence color ramps used to build the scale 
	\param nc		desired number of colors on the scale
	\param colors	resulting color scale
	\returns true if color scale was successfully generated and false otherwise
*/
bool getColors(std::vector<std::string>& ramps, int nc, std::vector<TeColor>& colors);
vector<TeColor> getColors(TeColor cfrom, TeColor cto, int nc);
vector<TeColor> getColors(vector<ColorBar>& iVec, int ncores);
string getColors(vector<ColorBar>& aVec, vector<ColorBar>& bVec, int groupingMode);
void generateColorBarMap(vector<ColorBar>& inputColorVec, int ncores, map<int, vector<TeColor> >& colorMap);
vector<ColorBar> getColorBarVector(string& scores, const bool& first);
unsigned int  TeReadColorRampTextFile(const string& fileName, map<string,string>& colorRamps);

#endif



