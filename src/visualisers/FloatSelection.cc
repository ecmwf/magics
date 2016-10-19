/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
