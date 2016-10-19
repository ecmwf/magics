/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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

void FlagPlotting::operator()(bool north, const PaperPoint& point, double x, double y,double val)
{ 
	   if (legendOnly_ )
		   return;
		double speed = sqrt((x*x)+(y*y));

		double value = this->value(x, y, val);
		 if ( !levels_->empty() && (value < levels_->front() || value  > levels_->back()) ) return;
		   if ( speed < min_speed_ || speed > max_speed_ ) return;
		
	   if ((*calm_)(point, x, y)) return;
	   Colour colour = this->colour(*colour_, x, y, val);
	   if ( north )
	   	   northFlag(colour)->push_back(ArrowPoint(x, y, point));
	   else
	   	   southFlag(colour)->push_back(ArrowPoint(x, y, point));

		
		
}
//! Setting for south Hemisphere wind
Flag* FlagPlotting::southFlag(const Colour& colour)
{
	map<Colour, Flag* >:: iterator flag =  southFlags_.find(colour);
	if ( flag !=southFlags_.end()  )
		return flag->second;

	 Flag* south = new Flag();
	   south->setColour(colour);
	   south->setThickness(thickness_);
	   south->setOriginHeight(origin_marker_size_);
	   south->setStyle(this->style_);
	   south->setCrossBoundary(cross_boundary_);
	   south->setOriginHeight(origin_marker_size_);
	   south->setHemisphere(SOUTH);
	   south->setLength(length_);
	   southFlags_.insert(make_pair(colour, south));
	   (*origin_).prepare(*south);
	   return south;
}


//! Setting for north Hemisphere wind
Flag* FlagPlotting::northFlag(const Colour& colour)
{
	map<Colour, Flag* >:: iterator flag =  northFlags_.find(colour);
	if ( flag != northFlags_.end()  )
	return flag->second;

	Flag *north = new Flag();
	north->setColour(colour);
	north->setOriginHeight(origin_marker_size_);
	north->setThickness(thickness_);
	north->setCrossBoundary(cross_boundary_);
	north->setOriginHeight(origin_marker_size_);
	north->setHemisphere(NORTH);
	northFlags_.insert(make_pair(colour, north));
	north->setLength(length_);
	(*origin_).prepare(*north);

    return north;
}


void FlagPlotting::prepare(BasicGraphicsObjectContainer& task, double)
{ 

    // Calm Indicator 
    (*calm_).colour(*colour_);
    (*calm_).height(calm_indicator_size_);
    (*calm_).below(calm_below_);
    (*calm_).prepare(task);
    
  
    


	if ( legendOnly_) {

		return;
	}
	// We also empty the containers to get a clean new plot
	northFlags_.clear();
	southFlags_.clear();

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

struct SortHelper
{
	SortHelper() {}
	~SortHelper() {}
	MAGICS_NO_EXPORT bool operator()(const Arrow* first, const Arrow* second)
	{
 		return first->size() > second->size();
	}
};
void FlagPlotting::finish(BasicGraphicsObjectContainer& out)
{
	if ( legendOnly_) {
		// and now we reset
		northFlags_.clear();
		southFlags_.clear();
		return;
	}

	vector<Flag*> flags;
	for ( map<Colour, Flag*>::iterator flag = northFlags_.begin(); flag != northFlags_.end(); ++flag)
		flags.push_back(flag->second);

	for ( map<Colour, Flag*>::iterator flag = southFlags_.begin(); flag != southFlags_.end(); ++flag)
		flags.push_back(flag->second);

	 std::sort(flags.begin(), flags.end(), SortHelper());

	// Now we feed the task...
	for (vector<Flag* >::iterator flag = flags.begin(); flag != flags.end(); ++flag) {
		if ( !(*flag)->empty() ) {

		     out.push_back(*flag);
		}
	}

}
