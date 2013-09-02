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

/*! \file AxisMethod.cc
    \brief Implementation of the Template class AxisMethod.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/

#include <limits>

#include "AxisMethod.h"
#include "Axis.h"
#include "Transformation.h"

using namespace magics;

AxisMethod::AxisMethod() 
{
}

AxisMethod::~AxisMethod() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisMethod::print(ostream& out)  const
{
	out << "AxisMethod[";
	out << "]";
}

void LogarithmicAxisMethod::prepare(const Axis& axis,  AxisItems& items)
{

	    vector<int> factor;
	    factor.push_back(1);
	    factor.push_back(2);
	    factor.push_back(5);

	    double min = axis.min();
	    double max = axis.max();

	    int s1 = (min < 0) ? -1: 1;
	    int s2 = (max < 0) ? -1: 1;


	    int log1 =  s1 *((min) ? log10(s1*min) : -5);
	    int log2 =  s2 *((max) ? log10(s2*max) : -5);

	    bool reduce = (min*max) < 0;

	    std::set<double> ticks;

	    double from = std::min(min, max);
	    double to = std::max(min, max);

	    for (int i = std::min(log1, log2); i <= std::max(log1, log2); i +=1 ) {
	        for ( vector<int>::iterator f = factor.begin(); f != factor.end(); ++f) {

	        	double x;
	        	if ( !reduce || ( reduce && i > -4)  ) {
	        		x = (*f) * exp10(i);
	        		if ( from <= x  && x <= to && x != 0 )
	        			ticks.insert(x);
	        		x = -x;
	        		if ( from <= x  && x <= to && x != 0 )
	        			ticks.insert(x);
	        	}
	        	if ( !reduce || ( reduce && -i > -4   )) {
	        		x = (*f) * exp10(-i);
	        		if ( from <= x  && x <= to && x != 0 )
	        			ticks.insert(x);
	        		x = -x;
	        		if ( from <= x  && x <= to && x != 0)
	                	ticks.insert(x);
	        	}
	        }
	    }



	for ( std::set<double>::const_iterator step = ticks.begin(); step != ticks.end(); ++step) {
		items.push_back(new AxisItem(*step));
	}
}

void  HyperAxisMethod::updateX(const Transformation& transformation)
{
	hyperMin_ = transformation.getDataVectorMinX();
	hyperMax_ = transformation.getDataVectorMaxX();
	
}

void  HyperAxisMethod::updateY(const Transformation& transformation)
{
	hyperMin_ = transformation.getDataVectorMinY();
	hyperMax_ = transformation.getDataVectorMaxY();
}

void HyperAxisMethod::prepare(const Axis& axis, AxisItems& items)
{
	double inc;
	int nb = 7;
	double step;
	double log, ws;
	bool horizontal = false;

	double min = hyperMin_.front();
	double max = hyperMax_.front();
	double wmax = std::max(hyperMin_.front(), hyperMax_.front());
	double wmin = std::min(hyperMin_.front(), hyperMax_.front());
	if ( wmin==wmax) {
		wmax = std::max(hyperMin_.back(), hyperMax_.back());
		wmin = std::min(hyperMin_.back(), hyperMax_.back());
		min = hyperMin_.back();
		max = hyperMax_.back();
		horizontal = true;
	}

	
	if (axis.interval_ == INT_MAX ) {
	
		while (nb < 10) {
			step = (wmax-wmin)/nb;
			log = log10(step);
			ws = pow(10., int(log));
			inc = ceil(step/ws)*ws;
			MagLog::dev() << "Automatic method ---> increment = " << inc << " ---> try base=" << inc/ws << endl;
			
			 if ( wmax-wmin/inc > 5 && (inc/ws == 1 || inc/ws == 2 || inc/ws == 3 || inc/ws == 5 || inc/ws == 10) ) {
				MagLog::dev() << "Automatic method ---> increment " << inc << " OK! " << endl;
				break;
			}
			nb++;
			
		}
	}
	
	else {
		inc = axis.interval_;
	}
	std::set<double> list;
	
	

    if ( min < max ) {
		double first = floor(min/inc) *inc;
		double last  = 0.;    
		int i = 0;
		for (double val = first;  val <= max; val = first + (inc*i) ) {
			if (val >= min && val <=max) {
				list.insert(value(val));
        		last = val;
			}
			i++;
		}
		list.insert(value(last+inc));
		
	}
	else {
		double first = floor(min/inc) *inc;    
		double last  = 0.;  
		int i = 0;
		for (double val = first;  val >= max; val = first - ( inc * i) ) {
			if (val >= wmin && val <= wmax) {
				list.insert(value(val));       		
        		last = val;
			}
			i++;
		}
		list.insert(value(last-inc));
	}
    
    double imin = hyperMin_[0];
    double imax = hyperMax_[0];
    double jmin = hyperMin_[1];
    double jmax = hyperMax_[1];;
    double iw = imax - imin;
    double jw = jmax - jmin;
    
    for (std::set<double>::iterator i = list.begin(); i != list.end(); ++i) {
    	vector<double> val;
    	if ( horizontal ) {
    		val.push_back(imin);

    		val.push_back(*i);
    		items.push_back( new AxisHyperItem(*i, val));
    	}
    	else {

        	val.push_back(*i);
        	double j = jmin+((*i)-imin)*(jw/iw);
        	val.push_back(j);
        	items.push_back( new AxisHyperItem(*i, val));
    	}
    }
}


void AxisMethod::prepare(const Axis& axis, AxisItems& items)
{
	double inc;
	int nb = 7;
	double step;
	double log, ws;


	double min = axis.min();
	double max = axis.max();

	double wmax = std::max(min, max);
	double wmin = std::min(min, max);

	bool automatic = false;
	
	if (axis.interval_ == INT_MAX ) {
		automatic = true;
		while (nb < 20) {
			step = (wmax-wmin)/nb;
			log = log10(step);
			ws = pow(10., int(log));
			inc = ceil(step/ws)*ws;
			MagLog::dev() << "Automatic method ---> increment = " << inc << " ---> try base=" << inc/ws << endl;
			
			 if ( wmax-wmin/inc > 5 && (inc/ws == 1 || inc/ws == 2 || inc/ws == 3 || inc/ws == 5 || inc/ws == 10) ) {
				MagLog::dev() << "Automatic method ---> increment " << inc << " OK! " << endl;
				break;
			}
			nb++;
			
		}
	}
	
	else {
		inc = axis.interval_;
	}
	std::set<double> list;

	if ( min < max )
	{
		double first = floor(min/inc) *inc;
		double last  = 0.;    
	    int i = 0;
		for (double val = first;  val <= max; val = first + (i*inc) ) {
			if (val >= min && val <=max) {
				list.insert(value(val));
        		last = val;

			}
			i++;
		}
		list.insert(value(last+inc));

	}
	else
	{
		double first = floor(min/inc) * inc;
		double last  = 0.;  
		int i = 0;
		for (double val = first;  val >= max; val = first - (i*inc)) {
			if (val >= wmin && val <= wmax) {
				list.insert(value(val));       		
        			last = val;
			}
			i++;
		}
		list.insert(value(last-inc));
	}
	std::set<double> slist;
	int mod  = (list.size() / 10);
	mod++;



	int i = 0;
	for ( std::set<double>::iterator e = list.begin(); e !=  list.end(); ++e) {
		if ( !automatic )
			slist.insert(*e);
		else if ( i %  mod == 0 )
			slist.insert(*e);

		i++;
	}


	// First we add the minor tich before the first one...
	std::set<double>::iterator front = slist.begin();
	double first = *front - inc;
	step = inc/(axis.minor_tick_count_+1);
	for ( double ii = first; ii < *front; ii+=step ) {
		items.push_back(new AxisMinorTickItem(ii));
	}

	double last = std::numeric_limits<double>::max();
	for (std::set<double>::iterator i = front; i != slist.end(); ++i) {
		// Add the minor Axis Items!!! 
		if ( last != std::numeric_limits<double>::max()) {
			double step = (*i-last)/(axis.minor_tick_count_+1);
			for ( double ii = last +step; ii < *i; ii+=step ) {
				items.push_back(new AxisMinorTickItem(ii));
			}
		}
		last = *i;
		addItem(items,*i);
	}
}

void AxisMethod::updateX(const Transformation& transformation)
{
	min_ = transformation.getMinX();
	max_ = transformation.getMaxX();



}

void AxisMethod::updateY(const Transformation& transformation)
{
	min_ = transformation.getMinY();
	max_ = transformation.getMaxY();

}

void     PositionListAxisMethod::prepare(const Axis& axis, AxisItems& items)
{
	for ( vector<double>::const_iterator tick = axis.positions_.begin(); tick != axis.positions_.end(); ++tick)
	{
		items.push_back(new AxisItem(*tick));
	}
}

