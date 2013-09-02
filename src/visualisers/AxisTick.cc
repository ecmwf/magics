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

/*! \file AxisTick.cc
    \brief Implementation of the Template class AxisTick.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/


#include "AxisTick.h"
#include "PaperPoint.h"
#include "Polyline.h"
#include "SceneVisitor.h"
#include "Transformation.h"

using namespace magics;

AxisTick::AxisTick() 
{
}


AxisTick::~AxisTick() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisTick::print(ostream& out)  const
{
	out << "AxisTick[";
	out << "]";
}

void AxisTick::vertical(const AxisItems& ticks, const Colour& colour, VerticalAxisVisitor& axis) 
{
	double x1, x2;
	axis.tick(x1, x2);
	
	const Transformation& transformation = axis.transformation();
	

	
	
	for (AxisItems::const_iterator y = ticks.begin(); y != ticks.end(); ++y)
    {    	    
	      
	        if ( !(*y)->filter(*this) ) continue;	        
	        if ( !transformation.inY((*y)->position()) ) continue;
	        
	        Polyline* tick = new Polyline();
	        PaperPoint p1 = PaperPoint(x1,  transformation.y((*y)->position()));
	        PaperPoint p2 = PaperPoint(x2, transformation.y((*y)->position()));
	        tick->push_back(p1);
	        tick->push_back(p2);
	        Colour calcol =  (colour_->automatic()) ? colour : *colour_;
	        tick->setColour(calcol);
	        tick->setThickness(thickness_);
	        axis.push_back(tick);
    }
    
	
	
	
}

void AxisTick::horizontal(const AxisItems& ticks, const Colour& colour, HorizontalAxisVisitor& axis) 
{
	
	double y1, y2;
	axis.tick(y1, y2);
		
		const Transformation& transformation = axis.transformation();
		
	for (AxisItems::const_iterator x = ticks.begin(); x != ticks.end(); ++x)
    {    	    
	      
	        if ( !(*x)->filter(*this) ) continue;
	      
	        if ( !transformation.inX((*x)->position()) ) continue;
	        Polyline* tick = new Polyline();
			double px = transformation.x((*x)->position());
	        tick->push_back(PaperPoint(px, y1));
	        tick->push_back(PaperPoint(px, y2));
	        Colour calcol =  (colour_->automatic()) ? colour : *colour_;
	        tick->setColour(calcol);
	        tick->setThickness(thickness_);
	        axis.push_back(tick);
    }
	
	
	   
}	

