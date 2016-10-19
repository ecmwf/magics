/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisGrid.cc
    \brief Implementation of the Template class AxisGrid.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 7-May-2004
    
    Changes:
    
*/



#include "AxisGrid.h"
#include "MagLog.h"
#include "PaperPoint.h"
#include "Polyline.h"
#include "Transformation.h"

using namespace magics;

AxisGrid::AxisGrid() 
{
}


AxisGrid::~AxisGrid() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisGrid::print(ostream& out)  const
{
	out << "AxisGrid[";
	AxisGridAttributes::print(out);
	out << "]";
}



void AxisGrid::vertical(const AxisItems& ticks, DrawingVisitor& out) const
{
	
	
	double bottom =out.minY();
	double top = out.maxY();
	double pos;
	
	const Transformation& transformation = out.transformation();
	for (AxisItems::const_iterator x = ticks.begin(); x != ticks.end(); ++x)
    {
    	if (!(*x)->filter(*this) ) continue;
    	pos = (*x)->position();
    	if ( !transformation.inX(pos) ) continue;
		Polyline* grid = new Polyline();
		grid->push_back(PaperPoint(transformation.x(pos), bottom));
		grid->push_back(PaperPoint(transformation.x(pos), top));
		if ( pos == reference_level_  ) {
			Colour colour = !reference_colour_->automatic() ? *reference_colour_ : *colour_;
			grid->setColour(colour);
			grid->setLineStyle(M_SOLID);
			grid->setThickness(reference_thickness_);
		}
		else {
			grid->setColour(*colour_);
			grid->setLineStyle(style_);
			grid->setThickness(thickness_);
		}
		
		out.push_back(grid);
    }
}

void AxisGrid::horizontal(const AxisItems& ticks, DrawingVisitor& out) const
{
	
	double left = out.minX();
	double right =  out.maxX();
	double pos;
	const Transformation& transformation = out.transformation();
	for (AxisItems::const_iterator y = ticks.begin(); y != ticks.end(); ++y)
    {
    	if (!(*y)->filter(*this) ) continue;
    	pos = (*y)->position();
    	if ( !transformation.inY(pos) ) continue;
    	Polyline* grid = new Polyline();
		grid->push_back(PaperPoint(left, transformation.y(pos)));
		grid->push_back(PaperPoint(right, transformation.y(pos)));
		if ( pos == reference_level_  ) {
				Colour colour = !reference_colour_->automatic() ? *reference_colour_ : *colour_;
				grid->setColour(colour);
				grid->setLineStyle(reference_style_);
				grid->setThickness(reference_thickness_);
		}
		else {
				grid->setColour(*colour_);
				grid->setLineStyle(style_);
				grid->setThickness(thickness_);
		}
		out.push_back(grid);
		 }
}

    

