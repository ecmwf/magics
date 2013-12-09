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

/*! \file ColourTechnique.cc
    \brief Implementation of the Template class ColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/



#include "ColourTechnique.h"
#include "LevelSelection.h"
#include "LegendVisitor.h"
#include "Polyline.h"
#include <limits>

using namespace magics;

ColourTechnique::ColourTechnique() 
{
}


ColourTechnique::~ColourTechnique() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ColourTechnique::print(ostream& out)  const
{
	out << "ColourTechnique[";
	out << "]";
}

void ColourTechnique::prepare(const LevelSelection& levels) 
{
	if (levels.empty() ) return; 
	clear();
	bands_.clear();
	ColourTable table;
	set(table, levels.size());
	
	if ( table.empty()) table.push_back(Colour("none"));
    ColourTable::ColourIterator colour = table.begin(); 
    
    Colour left("NONE");
    Colour right = colour->colour();

    double min = std::numeric_limits<double>::max();
    
    double previous = 0;
    int index = 0;
   
    for (LevelSelection::const_iterator  val  = levels.begin();  val != levels.end(); ++val)
    {               
    	
         (*this)[*val] = ColourInfo(index, *val, left, right);
         ranges_[*val] = std::make_pair(previous, *val);
         previous = *val;
         index++;
         if ( min != std::numeric_limits<double>::max() ) {
            bands_.insert(make_pair(Interval(min, *val), left));
            
            MagLog::dev() << min << "<<" << left << "<<" << *val << endl;
         }
         min = *val;
         left = right;            
         colour++;         
         if ( colour == table.end()) {
         	right = table.back().colour();
         	colour--;
         }
         else right = colour->colour();

    }
    bands_.insert(make_pair(Interval(levels.back(), levels.back() + epsilon), left));

    MagLog::dev() << levels.back() << "<<" << left << "<<" << levels.back() + epsilon << endl;
} 

Colour ColourTechnique::operator()(double value) const 
{

    const_iterator info = find(value);
    if (info == end() ) {

        //MagLog::warning() << "canot find a colour for " << value << "\n";
        return Colour(-1, -1, -1);
    }

    return info->second.left_;
} 

Colour ColourTechnique::left(double value) const 
{
    const_iterator info = find(value);
    if (info == end() ) {
        //MagLog::warning() << "canot find a colour for " << value << "\n";
        return Colour(-1,-1,-1);
    }
    return info->second.left_;
} 

Colour ColourTechnique::right(double value) const
{
    const_iterator info = find(value);
    if (info == end() ) {
        //MagLog::warning() << "canot find a colour for " << value << "\n";
        return Colour(-1,-1,-1);
    }
   
    return info->second.right_;
}
double ColourTechnique::leftRange(double value) const 
{
    map<double, pair<double, double> >::const_iterator info = ranges_.find(value);
    if (info == ranges_.end() ) {
        //MagLog::warning() << "canot find a colour for " << value << "\n";
        return INT_MAX;
    }
    return info->second.first;
} 

double ColourTechnique::rightRange(double value) const
{
    map<double, pair<double, double> >::const_iterator info = ranges_.find(value);
    if (info == ranges_.end() ) {
        //MagLog::warning() << "canot find a colour for " << value << "\n";
    	 return INT_MAX;
    }
    return  info->second.second;
}

Colour ColourTechnique::colour(double value) const 
{
 
    return bands_.find(value, Colour("none"));; 
} 


void ColourTechnique::colours(vector<string>& colours) const
{
	for ( IntervalMap<Colour>::const_iterator band = bands_.begin(); band != bands_.end(); ++band) 	
	    colours.push_back(band->second.name());
		
}

void ColourTechnique::visit(LegendVisitor& legend) 
{
	for ( IntervalMap<Colour>::const_iterator band = bands_.begin(); band != bands_.end(); ++band) 
	{
		double min = band->first.min_;
		double max = band->first.max_;
		Polyline* box = new Polyline();
		box->setColour(Colour("black"));
		box->setFilled(true);
		box->setFillColour(band->second);
		//MagLog::dev()<< "From " << min << " to " << max << "-->" << band->second << endl;
        FillShadingProperties* shading = new FillShadingProperties();
       
       
        box->setShading(shading);
     
        
        
        legend.add(new BoxEntry(min, max, box));
     
	}	
}


