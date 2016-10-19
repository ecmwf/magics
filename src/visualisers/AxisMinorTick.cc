/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisMinorTick.cc
    \brief Implementation of the Template class AxisMinorTick.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 15-Dec-2005
    
    Changes:
    
*/



#include "AxisMinorTick.h"
#include "PaperPoint.h"
#include "Polyline.h"
#include "Transformation.h"

using namespace magics;

AxisMinorTick::AxisMinorTick() 
{
}


AxisMinorTick::~AxisMinorTick() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisMinorTick::print(ostream& out)  const
{
	out << "AxisMinorTick[";
	out << "]";
}

void AxisMinorTick::vertical(const AxisItems& ticks, const Colour& colour, VerticalAxisVisitor& axis) 
{
	double x1, x2;	
	axis.minortick(x1, x2);
	
	const Transformation& projection = axis.transformation();
	
	for (AxisItems::const_iterator y = ticks.begin(); y != ticks.end(); ++y)
    {    	    
	      
	        if ( !(*y)->filter(*this) ) continue;
	        if ( !projection.inY((*y)->position()) ) continue;
	        Polyline* tick = new Polyline();
	        tick->push_back(PaperPoint(x1, (*y)->position()));
	        tick->push_back(PaperPoint(x2, (*y)->position()));
	        Colour calcol =  (colour_->automatic()) ? colour : *colour_;
	        tick->setColour(calcol);
	        tick->setThickness(thickness_);
	        axis.push_back(tick);
    }
      
}

void AxisMinorTick::horizontal(const AxisItems& ticks, const Colour& colour, HorizontalAxisVisitor& axis) 
{
	double y1, y2;
	axis.minortick(y1, y2);
	const Transformation& projection = axis.transformation();
	for (AxisItems::const_iterator x = ticks.begin(); x != ticks.end(); ++x)
    {    	    
	      
	        if ( !(*x)->filter(*this) ) continue;
	        if ( !projection.inX((*x)->position()) ) continue;
	        
	        Polyline* tick = new Polyline();

	        tick->push_back(PaperPoint((*x)->position(), y1));
	        tick->push_back(PaperPoint((*x)->position(), y2));
	        Colour calcol =  (colour_->automatic()) ? colour : *colour_;
	        tick->setColour(calcol);
	        tick->setThickness(thickness_);
	        axis.push_back(tick);
    }
}	

