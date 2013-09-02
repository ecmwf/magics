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

/*! \file LookupTableMode.cc
    \brief Implementation of the Template class LookupTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 17-May-2005
    
    Changes:
    
*/

#include "LookupTableMode.h"
#include "IntervalMap.h"

using namespace magics;

LookupTableMode::LookupTableMode() 
{
}


LookupTableMode::~LookupTableMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void LookupTableMode::print(ostream& out)  const
{
	out << "LookupTableMode[";
	LookupTableModeAttributes::print(out);
	out << "]";
}
FixedTableMode::FixedTableMode()
{
	
}


FixedTableMode::~FixedTableMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void FixedTableMode::print(ostream& out)  const
{
	out << "FixedTableMode[";
	FixedTableModeAttributes::print(out);
	LookupTableModeAttributes::print(out);
	out << "]";
}

void FixedTableMode::operator()(Image& im, Raster& rd)
{
     int 	i;		//auxiliary variables
     
     MagLog::dev() << *this << endl;

     // Initialize lookuptable
 //    ColourTable& table = im.getColourTable();
     //int nlevels = table.size();

    // Create a map of image levels interval
    IntervalMap<int> map;
    int level1 = levels_[0];

    for( i = 1; i < (int)levels_.size(); i++)
    {
         map[ Interval(level1, levels_[i]) ] = indexes_[i-1];
	 level1 = levels_[i];
    }
    map[Interval(level1, level1) ] = indexes_[indexes_.size()];

     // Create output image

     for (vector<double>::const_iterator val = rd.begin(); val != rd.end(); ++val)
     {
	     short ii = map.find(*val,0);
	     im.push_back(ii);
     }
     return;
}

