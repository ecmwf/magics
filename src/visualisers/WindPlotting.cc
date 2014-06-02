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

#include "WindPlotting.h"
#include "LegendVisitor.h"
#include "CustomisedPoint.h"
#include "Polyline.h"

using namespace magics;

map<string,  WindPlotting::AdvancedMethod > WindPlotting::methods_;
map<string,  WindPlotting::ColouringMethod > WindPlotting::colouringMethods_;
map<string,  WindPlotting::MinMaxMethod > WindPlotting::minMaxMethods_;

WindPlotting::WindPlotting() : legendOnly_(false)
{

	if ( methods_.empty() ) {
		methods_["on"] = &WindPlotting::advanced;
		methods_["advanced"] =&WindPlotting::advanced;
		methods_["off"] = &WindPlotting::off;
	}
	if ( minMaxMethods_.empty() ) {
			minMaxMethods_["on"] = &WindPlotting::advancedMinMax;
			minMaxMethods_["advanced"] =&WindPlotting::advancedMinMax;
			minMaxMethods_["off"] = &WindPlotting::offMinMax;
		}
	if ( colouringMethods_.empty() ) {
		colouringMethods_["speed"] = &WindPlotting::speed;
		colouringMethods_["parameter"] = &WindPlotting::parameter; 
	}
}

void WindPlotting::advancedMinMax(double& min, double& max)
{
	min = ( minSpeed() == -1.e21 ) ? min : minSpeed();
	max = ( maxSpeed() == 1.e21 ) ? max : maxSpeed();;
}
void WindPlotting::offMinMax(double& min, double& max)
{
	if ( min < minSpeed() )
		min = minSpeed();
	if ( max > maxSpeed() )
		max = maxSpeed();
}


void WindPlotting::adjust( CustomisedPointsList& points, const Transformation& transformation)
{
	//MagLog::dev() << "Data going from " << min << " to " << max << endl;
	
	vector<double> values;
	if (points.empty()) return;
	
	for (CustomisedPointsList::const_iterator point = points.begin(); point != points.end(); ++point) {
		if ( transformation.in((*point)->longitude(), (*point)->latitude()) ) 
			values.push_back(value((**point)["x_component"], (**point)["y_component"],  (**point)["colour_component"]));		
	}

	if (values.empty() ) return;

	double min = *std::min_element(values.begin(), values.end());
	double max = *std::max_element(values.begin(), values.end());
	cout << "min" << min << "    max " << max << endl;
	if ( same(min, max) ) return;
	string value = lowerCase(advanced_method_);
	map<string,  MinMaxMethod >::const_iterator method = minMaxMethods_.find(value);
	if  ( method ==  minMaxMethods_.end() ) {
		MagLog::warning() << advanced_method_ << " is not valid value for  'wind_advanced_method': reset to default[off]" << endl;
		offMinMax(min, max);
	}
	else
		(this->*method->second)(min, max);
	levels_->set(*this);	
	colourMethod_->set(*this);
	levels_->calculate(min, max, false);
	LevelSelection::const_iterator level = levels_->begin();
	colourMethod_->prepare(*levels_);
	Colour last;
	map_.clear();
	 while ( true) {
	    	if (level+1 == levels_->end() ) break;
	    	MagLog::debug() << "[" << *level << ", " << *(level+1) << "]=" << colourMethod_->right(*level) << endl;
	    	map_[ Interval(*level, *(level+1)) ] = colourMethod_->right(*level);
	    	last = colourMethod_->right(*level);
	    	++level;
	 }
	 // we add a last small one for the maax
	 map_[ Interval(levels_->back(), levels_->back() + epsilon) ] = last;

}


Colour& WindPlotting::colour(Colour& colour, double x, double y, double col)
{
	string value = lowerCase(advanced_method_);
	map<string,  AdvancedMethod >::const_iterator method = methods_.find(value);
	if  ( method == methods_.end() ) {
		MagLog::warning() << advanced_method_ << " is not valid value for  'wind_advanced_method': reset to default[off]" << endl;
		return off(colour, x, y, col);
	}
	else 
		return (this->*method->second)(colour, x, y, col);
}

double WindPlotting::parameter(double, double, double col)
{

	return col;
}

double WindPlotting::speed(double x, double y, double)
{
	return sqrt(x*x+y*y);
}

double WindPlotting::value(double x, double y, double col)
{
	string value = lowerCase(colour_method_);
	map<string,  ColouringMethod >::const_iterator method = colouringMethods_.find(value);
	if  ( method == colouringMethods_.end() ) {
			MagLog::warning() << colour_method_ << " is not valid value for  'wind_advanced_colour_parameter': reset to default[speed]" << endl;
			return speed( x, y, col);
	}
	else 
			return   (this->*method->second)(x, y,col);
}

Colour& WindPlotting::advanced(Colour& colour, double u, double v, double col)
{
	static Colour x("red");
	x = map_.find(value(u, v, col), colour);
	return x;
}
 
void WindPlotting::visit(LegendVisitor& legend)
{
	if ( !legend_ )
		return;

	if (magCompare(advanced_method_, "advanced") ||  magCompare(advanced_method_, "on") ) {
		if   ( map_.empty() ) {
			// no legend to plot
			return;
		}
		IntervalMap<Colour>::const_iterator interval;
		IntervalMap<Colour>::const_iterator last =  map_.end();
		--last;
		for ( interval = map_.begin(); interval != last; ++interval) {
			Polyline* box = new Polyline();

			double min =interval->first.min_;
			double max = interval->first.max_;

			box->setShading(new FillShadingProperties());

			box->setFillColour(interval->second);
			box->setFilled(true);

			legend.add(new BoxEntry(min, max, box));

		}
		legend.back()->last();
	}

}
