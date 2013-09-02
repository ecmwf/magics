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

/*! \file PolyShadingMethod.h
    \brief Definition of the Template class PolyShadingMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:cc
    
*/


#include "DotPolyShadingMethod.h"
#include "HatchPolyShadingMethod.h"
#include "LevelSelection.h"
#include "IsoPlot.h"
#include "PolyShadingTechnique.h"
using namespace magics;



	
    
void PolyShadingMethod::operator()(Polyline& poly) const
    {
       
		int index = poly.index();
		if (index < 0) 
			return;
        poly.setFilled(true);      
        poly.setStroke(false);
        poly.setFilled(true);
        poly.setFillColour(colours_[index]);
        FillShadingProperties* shading = new FillShadingProperties();                      
        poly.setShading(shading);
    };
   
void  PolyShadingMethod::visit(LegendVisitor& legend, const ColourTechnique& colour) {
        
        MagLog::dev() << "Create legend information"  << "\n";
        LegendEntryBuilder helper(legend, colour);
        std::adjacent_find(colour.begin(), colour.end(), LegendEntryBuilder(legend, this, colour));
        if ( colour.size() == 1 ) {
        		helper(*colour.begin(), *colour.begin());
        	}
        legend.last(); // Flag the last entry as being the last! To get a nice labelling in countinuous mode!!!
        
    }

int PolyShadingMethod::index(double value)
{


	if ( same(value, last_) )
		return indexes_.size() -1;
	return indexes_.find(value, -1);
}

int PolyShadingMethod::rightIndex(double value)
{
	if ( same(value, first_) ) return 0;
	if ( same(value, last_) ) return -1;
	return indexes_.find(value, -1);
}

int PolyShadingMethod::leftIndex(double value)
{
	if ( value < first_) return -1;
	if ( same(value, first_) ) return -1;
	if ( same(value, last_) )  return indexes_.size() -1;
	return indexes_.find(value, -1) - 1;

}
void PolyShadingMethod::prepare(const LevelSelection& levels, const ColourTechnique& colours)
{

	if (levels.empty() )return;
	first_ = levels.front();
	last_ =  levels.back();


	LevelSelection::const_iterator from = levels.begin();
	LevelSelection::const_iterator level = levels.begin();
	level++;



	indexes_.clear();
	colours_.clear();

	int index = 0;
	for (  ;  level != levels.end(); ++level) {
		indexes_.insert(make_pair(Interval(*from, *level), index));
		colours_.push_back(colours.right(*from));
		from++;
		index++;


	}

}



void DotPolyShadingMethod::prepare(const LevelSelection& levels, const ColourTechnique& colours)
{
	if (levels.empty() )return;

	float step = (max_density_ - min_density_)/(levels.size() - 1);
	first_ = levels.front();
	last_ =  levels.back();

	LevelSelection::const_iterator from = levels.begin();
	LevelSelection::const_iterator level = levels.begin();
	level++;
	float density = min_density_;
	int index = 0;
	indexes_.clear();
	colours_.clear();
	dots_.clear();
	for (  ;  level != levels.end(); ++level) {

		indexes_.insert(make_pair(Interval(*from, *level), index));
		colours_.push_back(colours.right(*from));
		dots_.push_back(density);
		from++;
		index++;
		density+=step;

	}

}


void  DotPolyShadingMethod::operator()(Polyline& poly) const {
    DotShadingProperties* shading = new DotShadingProperties();


    int index = poly.index();




    shading->size_ = size_;
    shading->density_ =  dots_[index];

    poly.setFilled(true);
    poly.setFillColour(colours_[index]);
	poly.setStroke(false);
    poly.setShading(shading);
    //MagLog::dev() << "Attach DotShading Information" << *shading << "\n";
}



void HatchPolyShadingMethod::prepare(const LevelSelection& levels, const ColourTechnique& colours) {
   	int index = 1;
   	if (index_ >= 7 || index_ <0) {
   		MagLog::warning() << "index should be < 7--> reset to 1 "<< endl;
   		index_ = 1 ;
   	}
   	first_ = levels.front();
	last_ =  levels.back();

   	LevelSelection::const_iterator from = levels.begin();
   	LevelSelection::const_iterator level = levels.begin();
   	indexes_.clear();
   	colours_.clear();
   	hatches_.clear();

	level++;
   	int i = 0;

   	for (  ;  level != levels.end(); ++level) {
   		indexes_.insert(make_pair(Interval(*from, *level), i));
		colours_.push_back(colours.right(*from));
   		hatches_.push_back((index_) ? index_ : index);
   		index++;
   		i++;
   		from++;
   		if ( index == 7 ) index = 1;
   	}



   }

void HatchPolyShadingMethod::operator()(Polyline& poly) const
{
       int index = poly.index();

       HatchShadingProperties* shading = new HatchShadingProperties();
       shading->index_     = hatches_[index];

       shading->density_   = density_;
       shading->thickness_ = thickness_;
       poly.setFilled(true);
       poly.setFillColour(colours_[index]);
       poly.setStroke(false);
       poly.setShading(shading);

}


