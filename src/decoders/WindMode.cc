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

/*! \file WindMode.cc
    \brief Implementation of the Template class WindMode.
    
    Magics Team - ECMWF 2006
    
    Started: Wed 9-Aug-2006
    
    Changes:
    
*/



#include "WindMode.h"

using namespace magics;

WindMode::WindMode() 
{
}


WindMode::~WindMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void WindMode::print(ostream& out)  const
{
	out << "WindMode[";
	out << "]";
}

void UVWindMode::x(Matrix** out1, Matrix** out2, Matrix* in1, Matrix* in2)
{
	*out1 = in1;
	*out2 = in2;
}


void SDWindMode::x(Matrix** out1, Matrix** out2, Matrix* in1, Matrix* in2)
{
	*out1 = in1;
	*out2 = in2;
        double x = 3.14/180.;
	vector<double>::const_iterator speed = in1->begin();
	vector<double>::const_iterator angle = in2->begin();
	vector<double> speeds;
	vector<double> directions;
//	MagLog::dev()<< "missing1-->" << in1->missing() << endl;
//	MagLog::dev()<< "missing2-->" << in2->missing() << endl;
	while ( speed != in1->end() && angle != in2->end() ) {
       if ( *speed == in1->missing() || *angle == in2->missing() ) 
       {
    	   speeds.push_back(in2->missing());
    	   directions.push_back(in2->missing());
       }
       else {
       	double a = 90 - (*angle); 
	a *= x;
       	speeds.push_back(*speed * -1 * cos(a));
    	directions.push_back(*speed*-1* sin(a));
       }
       speed++;
       angle++;
	}	
	
	(*out1)->clear();
	(*out2)->clear();
	
        vector<double>::iterator d = directions.begin();
        vector<double>::iterator send = speeds.end();
	for (vector<double>::iterator s = speeds.begin(); s != send; ++s) {
			(*out1)->push_back(*s);
			(*out2)->push_back(*d);
			++d;
	}
}


pair<double, double> SDWindMode::operator()(double s, double d)
{
	double a = 90 - (d);    
	double x = 3.14/180.;
	a *= x;
    return make_pair(s * -1 * cos(a), s*-1* sin(a)) ;
  
}


void VDWindMode::x(Matrix**, Matrix* in1, Matrix* in2)
{
}

void VDWindMode::y(Matrix**, Matrix* in1, Matrix* in2)
{
}


