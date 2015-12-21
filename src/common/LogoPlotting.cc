/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
	Symbol* symbol = new Symbol();

	double x =  (0.3 * 6.75 * 100) / tree.absoluteWidth();
	x = 90;
    double y = (0.3/tree.absoluteHeight())*100; 	 // 0.2

	(*symbol).setColour(Colour("BLUE"));
	(*symbol).setSymbol("logo_ecmwf");
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
