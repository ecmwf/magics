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

/*! \file WindPlotting.cc
    \brief Implementation of the Template class Wind.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#include "FlagPlotting.h"
#include "LegendVisitor.h"
#include "Histogram.h"

using namespace magics;

void FlagPlotting::operator()(bool north, const PaperPoint& point, double x, double y,double)
{ 
	   if (legendOnly_ )
		   return;
		double speed = sqrt((x*x)+(y*y));
		 if ( !levels_->empty() && (speed < levels_->front() || speed > levels_->back()) ) return;
		   if ( speed < min_speed_ || speed > max_speed_ ) return;
		
	   if ((*calm_)(point, x, y)) return;
	
		if ( north )
			northFlags_->push_back(ArrowPoint(x, y, point));
		else southFlags_->push_back(ArrowPoint(x, y, point));
		
		
}

void FlagPlotting::prepare(BasicGraphicsObjectContainer& task, double)
{ 

    // Calm Indicator 
    (*calm_).colour(*colour_);
    (*calm_).height(calm_indicator_size_);
    (*calm_).below(calm_below_);
    (*calm_).prepare(task);
    
  
    
    
    // Setting for south Hemisphere wind	
	southFlags_ = new Flag();
	southFlags_->setColour(*colour_);
	southFlags_->setThickness(thickness_);
	southFlags_->setStyle(style_);
	southFlags_->setCrossBoundary(cross_boundary_);
	southFlags_->setHemisphere(SOUTH);
	southFlags_->setOriginHeight(calm_indicator_size_);
	southFlags_->setLength(length_);
	
	// Setting for south Hemisphere wind	
	northFlags_ = new Flag();
	northFlags_->setColour(*colour_);
	northFlags_->setThickness(thickness_);
	northFlags_->setCrossBoundary(cross_boundary_);
	northFlags_->setOriginHeight(calm_indicator_size_);
	northFlags_->setHemisphere(NORTH);
	northFlags_->setLength(length_);
	
	// Origin Indicator 	  
    (*origin_).prepare(*southFlags_);
    (*origin_).prepare(*northFlags_);
    
    northFlags_->setOriginHeight(origin_marker_size_);
    southFlags_->setOriginHeight(origin_marker_size_);
	if ( legendOnly_) {

		return;
	}
	task.push_back(southFlags_);	
	task.push_back(northFlags_);	
}

void FlagPlotting::visit(LegendVisitor& legend)
{
	if ( !legend_ ) return;
	   
	   
	  
	   
	  
	   Flag* flags = new Flag();
	   flags->setColour(*colour_);
	   flags->setThickness(thickness_);
	   flags->setCrossBoundary(cross_boundary_);
	   (*origin_).prepare(*flags);
	   flags->setOriginHeight(origin_marker_size_);
	   flags->setHemisphere(NORTH);
	   flags->setLength(length_);

	   ostringstream text;

	   if (legend_text_ == "vector" ) legend_text_ = " ";
	   LegendEntry * entry = new FlagEntry(legend_text_, flags);

	   legend.add(entry);
	   legend.add(new EmptyEntry());
}

void FlagPlotting::visit(Data& data, PointsHandler& points, HistoVisitor& visitor)
{
    IntervalMap<Colour> beans;
    if ( !visitor.basic() ) {

	vector<double>::iterator from = levels_->begin();
        vector<double>::iterator to = levels_->begin();
        ++to;
        for (;  to != levels_->end(); ++to)
	{
	    	Colour colour;	    	
	    	if (magCompare(advanced_method_, "advanced") ||  
		    magCompare(advanced_method_, "on") ) 
		{	  	  	
			colour=colourMethod_->right(*from);
		}
		else
		{
		  	colour=*colour_;;
		} 	
            	beans.insert(make_pair(Interval(*from, *to), colour ));
            	++from;
        }		  	
    }
    
    Histogram helper;
    helper.visit(beans, data, points, visitor);
}  
