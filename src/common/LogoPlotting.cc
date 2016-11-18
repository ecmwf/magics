/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LogoPlotting.cc
    \brief Implementation of the Template class LogoPlotting.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 3-Jun-2005
    
    Changes:
    
*/

#include "LogoPlotting.h"
#include "Symbol.h"
#include "PaperPoint.h"
#include "ImportObject.h"
#include "BasicSceneObject.h"
#include "Dimension.h"

using namespace magics;



LogoPlotting::LogoPlotting()
{
	x_ = 80;
	y_ = 2.5;
}

LogoPlotting::~LogoPlotting()
{
}

/*!
 Class information are given to the output-stream.
*/		
void LogoPlotting::print(ostream& out)  const
{
	out << "LogoPlotting[";
	out << "]";
}

void LogoPlotting::operator()(BasicGraphicsObjectContainer& tree) const
{
	static map<string, string> logos;
	if ( logos.empty() ) {
		logos["ecmwf"] = "logo_ecmwf";
		logos["c3s"] = "logo_c3s";
		logos["cams"] = "logo_cams";
	}

	string logo = "logo_ecmwf";
	map<string, string>::iterator l = logos.find(name_);
	if ( l == logos.end() ) {
		MagLog::warning() << "Unknow logo " << name_ << ": use default " << name_ << endl;
	}

	Symbol* symbol = new Symbol();

	double x =  (0.3 * 6.75 * 100) / tree.absoluteWidth();
	x = 90;
    double y = (0.3/tree.absoluteHeight())*100; 	 // 0.2

	(*symbol).setColour(Colour("BLUE"));
	(*symbol).setSymbol(logo);
	(*symbol).setHeight(0.6);   // length = height * 6.75
	(*symbol).push_back(PaperPoint(x,y));

	tree.push_back(symbol);
}


UserLogoPlotting::UserLogoPlotting()
{
}

UserLogoPlotting::~UserLogoPlotting()
{
}

/*!
 Class information are given to the output-stream.
*/		
void UserLogoPlotting::print(ostream& out)  const
{
	out << "UserLogoPlotting[";
	out << "]";
}

void UserLogoPlotting::operator()(BasicGraphicsObjectContainer& tree) const
{
    ImportObject* object = new ImportObject();
    
	object->setPath(UserLogoPlottingAttributes::path_);
	object->setFormat(UserLogoPlottingAttributes::format_);
	object->setWidth(UserLogoPlottingAttributes::width_);
	object->setHeight(UserLogoPlottingAttributes::height_);
	
	double x = UserLogoPlottingAttributes::x_;
	double y = UserLogoPlottingAttributes::y_;

	const double height = tree.absoluteHeight();    
	const double width = tree.absoluteWidth();
	// Dimensions
	if ( UserLogoPlottingAttributes::bottom_.empty() || UserLogoPlottingAttributes::left_.empty() ) {
		if ( magCompare(UserLogoPlottingAttributes::units_, "cm") ) {
			
			x = (UserLogoPlottingAttributes::x_/width)*100;
			y = (UserLogoPlottingAttributes::y_/height)*100;
		}
	}
	else {
		Dimension bottom(bottom_ , height, 2.5);
		Dimension left(left_ , width, 80);
		x = left.percent();
		y = bottom.percent();
	}
	object->setOrigin(PaperPoint(x, y));
	tree.push_back(object);    
}
