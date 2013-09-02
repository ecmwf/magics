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

/*! \file IntervalSelectionType.cc
    \brief Implementation of the Template class IntervalSelectionType.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
    
*/



#include "IntervalSelectionType.h"
#include "UserPoint.h"
#include "PointsHandler.h"

using namespace magics;

IntervalSelectionType::IntervalSelectionType() 
{
}


IntervalSelectionType::~IntervalSelectionType() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void IntervalSelectionType::print(ostream& out)  const
{
	out << "IntervalSelectionType[";
    IntervalSelectionTypeAttributes::print(out);
	out << "]";
}

void IntervalSelectionType::calculate(double min, double max, bool shading) 
{
	
    clear();
    //double maxi = data.max();
    //double mini = data.min();
	std::set<double> levels;
	
	double lmax, lmin;
	if ( shading) {
	    if (  same(max_,1.0e+21) ) {
	    	max_ = max_shade_;    	
	    }
	    if (  same(min_, -1.0e+21) ) {
	    	min_ = min_shade_;   	
	    }
	}
    

       lmax = 	same(max_,1.0e+21) ?  max :max_;
       lmin = 	same(min_,-1.0e+21) ?  min :min_;

   
   
       
    levels.insert(lmax);
    levels.insert(lmin);
    
    
    double level = reference_;
    int i = 0;
    while ( level < lmax && !same(level, lmax) ) {
    	if ( level > lmin )
    		levels.insert(level);
        level = reference_ + (i * interval_);
        i++;
    }
    level = reference_;
    i = 1;
    while ( level > lmin &&   !same(level, lmin) ) {
        if ( level < lmax )
        	levels.insert(level);
        level = reference_ - (i * interval_);
        i++;
    }

    ostringstream out;
    out << "\nIntervalSelectionType-->[";
    for (std::set<double>::const_iterator level = levels.begin(); level != levels.end(); ++level) {
    	out  << *level << ", ";
    	push_back(*level);
    }
    out << "]" << endl;
    MagLog::dev()  << out.str() << endl;
    	
    

}




