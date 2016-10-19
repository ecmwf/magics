/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotItem.cc
    \brief Implementation of the Template class BoxPlotBox.
    
    Magics Team - ECMWF 2006
    
    Started: Fri 29-Sep-2006
    
    Changes:
    
*/

#include "CustomisedPoint.h"
#include "BoxPlotItem.h"
#include "Polyline.h"
#include "Transformation.h"
using namespace magics;

void BoxPlotBox::operator()(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const
{
	const Transformation& transformation = visitor.transformation();
	Polyline* box  = new Polyline();				
	box->setFilled(true);	
	box->setFillColour(*colour_);
	box->setShading(new FillShadingProperties());
	
	double width = (width_* cm_)/2; // Could later be expressed in %
	CustomisedPoint::const_iterator upper = point.find("upper");
	CustomisedPoint::const_iterator lower = point.find("lower");
	CustomisedPoint::const_iterator x = point.find("x");
	if ( x == point.end() || upper == point.end() || lower == point.end() ) return;
	box->push_back(transformation(UserPoint(x->second-width, upper->second)));
	box->push_back(transformation(UserPoint(x->second+width, upper->second)));
	box->push_back(transformation(UserPoint(x->second+width, lower->second)));
	box->push_back(transformation(UserPoint(x->second-width, lower->second)));
	box->push_back(transformation(UserPoint(x->second-width, upper->second)));

	(*border_)(*box);
	visitor.push_back(box);

	CustomisedPoint::const_iterator median = point.find("median");

	if ( median == point.end() ) return;

	Polyline* line  = new Polyline();	

	line->push_back(transformation(UserPoint(x->second-width, median->second)));
	line->push_back(transformation(UserPoint(x->second+width, median->second)));

	(*median_)(visitor, line);
}

void BoxPlotBoxBorder::operator()(Polyline& box) const
{
	box.setColour(*colour_);
	box.setLineStyle(style_);
	box.setThickness(thickness_);
		
}

void BoxPlotMedian::operator()(BasicGraphicsObjectContainer& visitor, Polyline* line) const
{
	
	line->setColour(*colour_);
	line->setLineStyle(style_);
	line->setThickness(thickness_);
	visitor.push_back(line);
}


void BoxPlotWhiskerBox::top(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const
{
	const Transformation& transformation = visitor.transformation();
	Polyline* whisker  = new Polyline();			
	whisker->setFilled(true);	
	whisker->setFillColour(*colour_);
	whisker->setShading(new FillShadingProperties());
	double width = (width_* cm_)/2; // Could later be expressed in %
	CustomisedPoint::const_iterator max = point.find("max");
	CustomisedPoint::const_iterator upper = point.find("upper");
	CustomisedPoint::const_iterator x = point.find("x");
	if ( x == point.end() || max == point.end() || upper == point.end() ) return;
	whisker->push_back(transformation(UserPoint(x->second-width, max->second)));
	whisker->push_back(transformation(UserPoint(x->second+width, max->second)));
	whisker->push_back(transformation(UserPoint(x->second+width, upper->second)));
	whisker->push_back(transformation(UserPoint(x->second-width, upper->second)));
	whisker->push_back(transformation(UserPoint(x->second-width, max->second)));

	(*border_)(*whisker);
	visitor.push_back(whisker);
	
}

void BoxPlotWhiskerBox::bottom(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const
{
	const Transformation& transformation = visitor.transformation();
	Polyline* whisker  = new Polyline();			
	whisker->setFilled(true);	
	whisker->setFillColour(*colour_);
	whisker->setShading(new FillShadingProperties());
	double width = (width_* cm_)/2; // Could later be expressed in %
	CustomisedPoint::const_iterator min = point.find("min");
	CustomisedPoint::const_iterator lower = point.find("lower");
	CustomisedPoint::const_iterator x = point.find("x");
	if ( x == point.end() || min == point.end() || lower == point.end() ) return;
	whisker->push_back(transformation(UserPoint(x->second-width, min->second)));
	whisker->push_back(transformation(UserPoint(x->second+width, min->second)));
	whisker->push_back(transformation(UserPoint(x->second+width, lower->second)));
	whisker->push_back(transformation(UserPoint(x->second-width, lower->second)));
	whisker->push_back(transformation(UserPoint(x->second-width, min->second)));

	(*border_)(*whisker);
	visitor.push_back(whisker);
}

void BoxPlotWhiskerLine::top(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const
{
	const Transformation& transformation = visitor.transformation();
	Polyline* whisker  = new Polyline();				
	whisker->setColour(*colour_);
	whisker->setLineStyle(style_);
	whisker->setThickness(thickness_);

	CustomisedPoint::const_iterator max = point.find("max");
	CustomisedPoint::const_iterator upper = point.find("upper");
	CustomisedPoint::const_iterator x = point.find("x");
	if ( x == point.end() || max == point.end() || upper == point.end() ) return;
	whisker->push_back(transformation(UserPoint(x->second, max->second)));
	whisker->push_back(transformation(UserPoint(x->second, upper->second)));
	

	
	visitor.push_back(whisker);
}

void BoxPlotWhiskerLine::bottom(BasicGraphicsObjectContainer& visitor, const CustomisedPoint& point) const
{
	const Transformation& transformation = visitor.transformation();
	Polyline* whisker  = new Polyline();			
	whisker->setColour(*colour_);
	whisker->setLineStyle(style_);
	whisker->setThickness(thickness_);

	CustomisedPoint::const_iterator min = point.find("min");
	CustomisedPoint::const_iterator lower = point.find("lower");
	CustomisedPoint::const_iterator x = point.find("x");
	if ( x == point.end() || min == point.end() || lower == point.end() ) return;
	whisker->push_back(transformation(UserPoint(x->second, min->second)));
	whisker->push_back(transformation(UserPoint(x->second, lower->second)));
	

	
	visitor.push_back(whisker);
}

void BoxPlotWhiskerBorder::operator()(Polyline& whisker) const
{
	whisker.setColour(*colour_);
	whisker.setLineStyle(style_);
	whisker.setThickness(thickness_);
}


