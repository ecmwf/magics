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
#include "LegendVisitor.h"
#include "ArrowPlotting.h"
#include "Histogram.h"

using namespace magics;

void ArrowPlotting::operator()(bool north, const PaperPoint& point, double x, double y, double val)
{ 
   const double speed = this->speed(x, y, val);
   if ( maxVelocity_ < speed )
       maxVelocity_ = speed;

   if ( !levels_->empty() && (speed < levels_->front() || speed > levels_->back()) ) return;
   if ( speed < min_speed_ || speed > max_speed_ ) return;

   if ((*this->calm_)(point, x, y)) return; 

   Colour colour = this->colour(*colour_, x, y, val);
   if ( north )
	   northArrow(colour)->push_back(ArrowPoint(x, y, point));
   else 
	   southArrow(colour)->push_back(ArrowPoint(x, y, point));
}



//! Setting for south Hemisphere wind	
Arrow* ArrowPlotting::southArrow(const Colour& colour)
{
	map<Colour, Arrow* >:: iterator arrow =  southArrows_.find(colour);
	if ( arrow !=southArrows_.end()  ) 
		return arrow->second;

	 Arrow * south = new Arrow();
	   south->setColour(colour);
	   south->setThickness(this->thickness_);
	   south->setStyle(this->style_);
	   south->setScale(this->unit_velocity_);
	   south->setHemisphere(SOUTH);
	   south->setArrowPosition(this->origin_position_);
	   south->setHeadIndex(this->head_);
	   south->setHeadRatio(this->ratio_);
	   southArrows_.insert(make_pair(colour, south));
	   return south;
}


//! Setting for north Hemisphere wind
Arrow* ArrowPlotting::northArrow(const Colour& colour)
{
	map<Colour, Arrow* >:: iterator arrow =  northArrows_.find(colour);
	if ( arrow != northArrows_.end()  ) 
	return arrow->second;

	Arrow *north = new Arrow();
	north->setColour(colour);
	north->setThickness(this->thickness_);
	north->setStyle(this->style_);
	north->setScale(this->unit_velocity_);
	north->setHemisphere(NORTH);
	north->setArrowPosition(this->origin_position_);
	north->setHeadIndex(this->head_);
	north->setHeadRatio(this->ratio_); 
	northArrows_.insert(make_pair(colour, north));
	return north;
}

void ArrowPlotting::prepare(BasicGraphicsObjectContainer& out)
{
   // Calm Indicator 
   (*this->calm_).colour(*this->colour_);
   (*this->calm_).height(this->calm_indicator_size_);
   (*this->calm_).below(this->calm_below_);
   (*this->calm_).prepare(out);
   // We also empty the containers to get a clean new plot
   northArrows_.clear();
   southArrows_.clear();
}

void ArrowPlotting::prepare(BasicGraphicsObjectContainer& out, double res)
{
   // Calm Indicator 
   (*this->calm_).colour(*this->colour_);
   (*this->calm_).height(this->calm_indicator_size_);
   (*this->calm_).below(this->calm_below_);
   (*this->calm_).prepare(out);

   if ( magCompare(unit_system_, "automatic") ) {
	   // adjust the scaling factor!

	   this->unit_velocity_ /= res;
	   MagLog::dev() << "unit_velocity_-->" << this->unit_velocity_ << endl;
   }
   else if ( magCompare(unit_system_, "paper") ) {
	// adjust the scaling factor!
	//this->unit_velocity_ /= out.transformation().unitToCm(out.absoluteWidth(), out.absoluteHeight());  			
	MagLog::dev() << "unit_velocity_-->" << this->unit_velocity_ << endl;
   }

   this->maxVelocity_ = -1;

   // We also empty the containers to get a clean new plot
   northArrows_.clear();
   southArrows_.clear();

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

void ArrowPlotting::finish(BasicGraphicsObjectContainer& out)
{	
	if ( legendOnly_) {
		// and now we reset
		northArrows_.clear();
		southArrows_.clear();
		return;
	}
	unit_velocity_ = (  unit_velocity_ == 0 ) ? maxVelocity_ :  unit_velocity_;
	vector<Arrow*> arrows;
	for ( map<Colour, Arrow*>::iterator arrow = northArrows_.begin(); arrow != northArrows_.end(); ++arrow) 
		arrows.push_back(arrow->second);
	
	for ( map<Colour, Arrow*>::iterator arrow = southArrows_.begin(); arrow != southArrows_.end(); ++arrow) 
		arrows.push_back(arrow->second);
	
	 std::sort(arrows.begin(), arrows.end(), SortHelper());

	// Now we feed the task...     
	for (vector<Arrow* >::iterator arrow = arrows.begin(); arrow != arrows.end(); ++arrow) {
		if ( !(*arrow)->empty() ) {
		     (*arrow)->setScale(this->unit_velocity_);
		     out.push_back(*arrow);
		}
	}

}


void ArrowPlotting::visit(LegendVisitor& legend)
{
   if ( !this->legend_ ) return;

   WindPlotting::visit(legend);
   // const double scale = legend.transformation().unitToCm(legend.absoluteWidth(), legend.absoluteHeight());
   const double scale= 1.;

   const double speed = (this->unit_velocity_) ? this->unit_velocity_ : this->maxVelocity_;
   Arrow* arrow = new Arrow();
   (*arrow).setColour(*this->colour_);
   arrow->setThickness(this->thickness_);
   arrow->setScale(scale);

   arrow->setArrowPosition(this->origin_position_);
   arrow->setHeadIndex(this->head_);
   arrow->setHeadRatio(this->ratio_);

   if ( legend_text_.empty() || legend_text_ == "vector" ) {
	ostringstream text;
   	text <<  speed << " "<< legend_unit_;
   	legend_text_ = text.str();
   }

   LegendEntry * entry = new ArrowEntry(legend_text_, arrow);

   legend.add(entry);
   legend.add(new EmptyEntry());
}


void ArrowPlotting::visit(Data& data, PointsHandler& points, HistoVisitor& visitor)
{
    IntervalMap<Colour> beans;
    if ( !visitor.basic() ) {

	vector<double>::iterator from = this->levels_->begin();
        vector<double>::iterator to = this->levels_->begin();
        ++to;
        for (;  to != this->levels_->end(); ++to)
	{
	    	Colour colour;	    	
	    	if (magCompare(advanced_method_, "advanced") ||  
		    magCompare(advanced_method_, "on") ) 
		{	  	  	
			colour=colourMethod_->right(*from);
		}
		else
		{
		  	colour=*this->colour_;;
		} 	
            	beans.insert(make_pair(Interval(*from, *to), colour ));
            	++from;
        }		  	
    }
    
    Histogram helper;
    helper.visit(beans, data, points, visitor);
}  
