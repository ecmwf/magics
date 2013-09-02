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

/*! \file HeightSelection.cc
    \brief Implementation of the Template class HeightSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/



#include "HeightTechnique.h"
#include "LevelSelection.h"

using namespace magics;

HeightTechnique::HeightTechnique() 
{
}


HeightTechnique::~HeightTechnique() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void HeightTechnique::print(ostream& out)  const
{
	out << "HeightTechnique[";
	out << "]";
}

void HeightTechnique::set(const HeightTechniqueInterface&) 
{
    
}


ListHeightTechnique::ListHeightTechnique() 
{
}


ListHeightTechnique::~ListHeightTechnique() 
{
}

void ListHeightTechnique::set(const HeightTechniqueInterface& interface) 
{
	   list_ = interface.getHeights();
	   policy_ = interface.getHeightPolicy();
}

void ListHeightTechnique::prepare(LevelSelection& levels) 
{
	heights_.clear();
	if ( list_.empty() ) list_.push_back(0.2);
	if ( levels.size() == 1) {
		heights_[Interval(levels.front(), levels.front())] = list_.front();
		return;
	}
	
	LevelSelection::const_iterator level = levels.begin();
	vector<double>::const_iterator height = list_.begin();
	while (true) {
		heights_[Interval(*level, *(level+1))] = *height;
		height++;
		level++;
		if ( level == levels.end() ) break;
		if ( height == list_.end() ) {
			if (policy_ != M_LASTONE) 
				height =  list_.begin();
			else --height;
		}
	}
}

CalculateHeightTechnique::CalculateHeightTechnique() 
{
}


CalculateHeightTechnique::~CalculateHeightTechnique() 
{
}

void CalculateHeightTechnique::set(const HeightTechniqueInterface& interface) 
{
		min_ = interface.getMinHeight();
		max_ = interface.getMaxHeight();
		
		
   
}

void CalculateHeightTechnique::prepare(LevelSelection& levels) 
{
	assert(levels.size() > 1);
	heights_.clear();
	double step = (levels.size() == 2) ?  (max_ - min_)  : (max_ - min_) / (levels.size() - 2);
	LevelSelection::const_iterator level = levels.begin();
	double height = min_;
	
		while (true) {
			MagLog::debug() << "[" << *level << ", " << *(level+1) << "]=" << height << "(height)" << endl;
			heights_[Interval(*level, *(level+1))] = height;
			height += step;			
			level++;
			if ( level == levels.end() ) break;			
		}
}
