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

/*! \file FloatSelection.cc
    \brief Implementation of the Template class FloatSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/
#include "FloatSelection.h"

#include <cmath>

using namespace magics;

FloatSelection::FloatSelection() 
{
}


FloatSelection::~FloatSelection() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void FloatSelection::print(ostream& out)  const
{
	out << "FloatSelection[";
	out << "]";
}

void FloatSelection::calculate(double min, double max, int count)
{
    
        double step = (max - min)/count;

        double log = log10(step);
        double istep = pow(10., int(log));	
        double inc = ceil(step/istep)*istep;
        double first = floor(min/inc)*inc;

        if (first < min) first += inc;        
        for (double val = first; val <= max; val += inc) 
            push_back(val);                
}

void FloatSelection::calculate(const doublearray& list)
{
    for (doublearray::const_iterator val = list.begin(); val != list.end(); ++val) {
        push_back(*val);
    }
                  
}
