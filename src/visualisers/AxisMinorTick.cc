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

