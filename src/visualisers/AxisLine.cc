/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisLine.cc
    \brief Implementation of the Template class AxisLine.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/


#include "AxisLine.h"
#include "PaperPoint.h"
#include "Polyline.h"
#include "Polyline.h"
#include "Transformation.h"
using namespace magics;

AxisLine::AxisLine() 
{
}


AxisLine::~AxisLine() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisLine::print(ostream& out)  const
{
	out << "AxisLine[";
	out << "]";
}

Colour AxisLine::colour()
{
	return colour_->automatic() ? *default_colour_ : *colour_; 
}

Polyline* AxisLine::line() const 
{
	Polyline* axis = new Polyline();
		  
		    	axis->setColour(colour_->automatic() ? *default_colour_ : *colour_);
		    	axis->setThickness(thickness_);
		    	axis->setLineStyle(style_);
     return axis;
}

void AxisLine::horizontal(TopAxisVisitor& out) const
{	
	    PaperPoint from(out.minX(), out.minY());
	    PaperPoint to(out.maxX(), out.minY());
	    	
	    Polyline* axis = line();
	    axis->push_back(from);
	    axis->push_back(to);
	    out.push_back(axis);
}

void AxisLine::horizontal(BottomAxisVisitor& out) const
{
	PaperPoint from(out.minX(), out.maxY());
		    PaperPoint to(out.maxX(), out.maxY());
		    	
		    Polyline* axis = line();
		    axis->push_back(from);
		    axis->push_back(to);
		    out.push_back(axis);
}





void AxisLine::vertical(LeftAxisVisitor& out) const
{
	PaperPoint from(out.maxX(), out.minY());
			    PaperPoint to(out.maxX(), out.maxY());
			    	
			    Polyline* axis = line();
			    axis->push_back(from);
			    axis->push_back(to);
			    out.push_back(axis);
}

void AxisLine::vertical(RightAxisVisitor& out) const
{
	PaperPoint from(out.minX(), out.minY());
			    PaperPoint to(out.minX(), out.maxY());
			    	
			    Polyline* axis = line();
			    axis->push_back(from);
			    axis->push_back(to);
			    out.push_back(axis);
}


