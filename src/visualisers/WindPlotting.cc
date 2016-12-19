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

#include "WindPlotting.h"
#include "LegendVisitor.h"
#include "CustomisedPoint.h"
#include "Polyline.h"

using namespace magics;

map<string,  WindPlotting::AdvancedMethod > WindPlotting::methods_;
map<string,  WindPlotting::ColouringMethod > WindPlotting::colouringMethods_;
map<string,  WindPlotting::MinMaxMethod > WindPlotting::minMaxMethods_;
map<string,  WindPlotting::SettingMethod > WindPlotting::settingMethods_;

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
	if ( settingMethods_.empty() ) {
			settingMethods_["on"] = &WindPlotting::setAdvanced;
			settingMethods_["advanced"] =&WindPlotting::setAdvanced;
			settingMethods_["off"] = &WindPlotting::setNormal;
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
void WindPlotting::setAdvanced(double& min, double& max)
{

	advancedMinMax(min, max);
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
	map_[ Interval(levels_->back(), levels_->back() + EPSILON) ] = last;

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

	string value = lowerCase(advanced_method_);



	map<string,  SettingMethod >::const_iterator set = settingMethods_.find(value);
		if  ( set ==  settingMethods_.end() ) {
			MagLog::warning() << advanced_method_ << " is not valid value for  'wind_advanced_method': reset to default[off]" << endl;
			setNormal(min,max);
		}
		else
			(this->*set->second)(min, max);

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
		legend.newLegend();
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
