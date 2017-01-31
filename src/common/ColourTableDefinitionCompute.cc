/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionCompute.cc
    \brief Implementation of the Template class ColourTableDefinitionCompute.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/



#include "ColourTableDefinitionCompute.h"
#include "XmlNode.h"

using namespace magics;

ColourTableDefinitionCompute::ColourTableDefinitionCompute() 
{
  methods_["anti_clockwise"] = &ColourTableDefinitionCompute::hsl;
  methods_["clockwise"] = &ColourTableDefinitionCompute::hsl;
  methods_["linear"] = &ColourTableDefinitionCompute::linear;

}

ColourTableDefinitionCompute::ColourTableDefinitionCompute(const string& min, const string& max, const string& direction) : 
  minColour_(min), maxColour_(max), direction_(direction)
{

}


ColourTableDefinitionCompute::~ColourTableDefinitionCompute() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ColourTableDefinitionCompute::print(ostream& out)  const
{
	out << "ColourTableDefinitionCompute[";
	out << "]";
}

void ColourTableDefinitionCompute::set(const ColourTableDefinitionComputeInterface& attributes)
{
	minColour_ = attributes.getMin();
	maxColour_ = attributes.getMax();
	direction_ = attributes.getDirection();
}

void ColourTableDefinitionCompute::set(const XmlNode& node)
{
	direction_ = node.getAttribute("direction");
	MagLog::warning() << "ColourTableDefinitionCompute::set(const XmlNode&): to be implemented\n";
	
	for (XmlNode::ElementIterator elt = node.firstElement(); elt != node.lastElement(); ++elt) {
		if ( magCompare((*elt)->name(), "min_colour") ) {
			minColour_ = Colour((*elt)->data());
		}
		if ( magCompare((*elt)->name(), "max_colour") ) {
			maxColour_ = Colour((*elt)->data());
		}
		
	}
	
	
}


void ColourTableDefinitionCompute::hsl(ColourTable& table, int nb)
{
  double step_hue;
  double step_light;
  double step_alpha;
  Hsl hmin = minColour_.hsl();
  Hsl hmax = maxColour_.hsl();
  step_light = (hmax.light_ - hmin.light_)/(nb-2);
  step_alpha = (hmax.alpha_ - hmin.alpha_)/(nb-2);
  if ( magCompare(direction_, "anti_clockwise") ) {
     if ( hmax.hue_ < hmin.hue_ )  hmax.hue_ += 360;
     step_hue = (hmax.hue_ - hmin.hue_)/(nb-2);
  } 
  else {
      if ( hmin.hue_ < hmax.hue_ )  hmin.hue_ += 360;
      step_hue =  (hmax.hue_ - hmin.hue_)/(nb-2);
  }

    
  float step_sat =  (hmax.saturation_ - hmin.saturation_)/(nb-2);
  // WE have nb levels : we need nb-1 colours! 
 
  for ( int i = 0;  i < nb-1; i++) {
    MagLog::dev() << "ColourTableDefinitionCompute::set->add-->" << Colour(hmin) << endl;
     table.push_back(Colour(hmin));
     hmin.saturation_ += step_sat;
     hmin.hue_ += step_hue;
     hmin.light_ += step_light;
     hmin.alpha_ += step_alpha;
  }
}

void ColourTableDefinitionCompute::linear(ColourTable& table, int nb)
{
  double step_red;
  double step_green;
  double step_blue;
  double step_alpha;

  step_red = (maxColour_.red() - minColour_.red())/(nb-2);
  step_green = (maxColour_.green() - minColour_.green())/(nb-2);
  step_blue = (maxColour_.blue() - minColour_.blue())/(nb-2);
  step_alpha = (maxColour_.alpha() - minColour_.alpha() )/(nb-2);
  double red = minColour_.red();
  double green = minColour_.green();
  double blue = minColour_.blue();
  double alpha = minColour_.alpha();

  for ( int i = 0;  i < nb-1; i++) {
     table.push_back(Colour(red, green, blue, alpha));
     
     red += step_red;
     green += step_green;
     blue += step_blue;
     alpha += step_alpha;
  }

}

void ColourTableDefinitionCompute::set(ColourTable& table, int nb)
{
	prepare();
	
	MagLog::dev() << "ColourTableDefinitionCompute::set->min-->" << minColour_ << endl;
	MagLog::dev() << "ColourTableDefinitionCompute::set->max-->" << maxColour_ << endl;
	MagLog::dev() << "nb interval-->" << nb << endl;
  
    if (nb == 1) {
       	table.push_back(minColour_);
       	return;
       }
    if (nb == 2) {
    	table.push_back(minColour_);
    	return;
    }
    if (nb == 3) {
       	table.push_back(minColour_);
       	table.push_back(maxColour_);
       	return;
       }
  std::map<string, ComputeFunction>::iterator method = methods_.find(lowerCase(direction_));
  if ( method == methods_.end() ) 
      linear(table, nb);
  else
      (this->*method->second)(table, nb);
  
   
}


