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

/*! \file SimplePolylineInput.cc
    \brief Implementation of the Template class SimplePolylineInput.
    
    Magics Team - ECMWF 2007
    
    Started: Mon 19-Mar-2007
    
    Changes:
    
*/



#include "SimplePolylineInput.h"

using namespace magics;

SimplePolylineInput::SimplePolylineInput() 
{
}


SimplePolylineInput::~SimplePolylineInput() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void SimplePolylineInput::print(ostream& out)  const
{
	out << "SimplePolylineInput[";
	out << "]";
}

void SimplePolylineInput::decode()
{
	if ( latitudes_.empty() && longitudes_.empty() && values_.empty() ) {
		ifstream position(position_filename_.c_str());
		double lat, lon;
		if(position)
		{	
			while (!position.eof()) {
				position >> lon >> lat;
                if (!position.eof())
                {
				    latitudes_.push_back(lat);
				    longitudes_.push_back(lon);
                }			
			}	
		}

		ifstream values(values_filename_.c_str());
		double value;
		if(values)
		{	
			while (!values.eof()) {
				values >> value;
                if (!values.eof())
                {
				    values_.push_back(value);
                }
			}	
		}
	}
		
    floatarray::const_iterator lat = latitudes_.begin();
    floatarray::const_iterator lon = longitudes_.begin();
    floatarray::const_iterator val = values_.begin();
    
    int num_polylines = 0;

    if (!values_.empty())
    {
        while ( lat != latitudes_.end() && lon != longitudes_.end() ) 
        {
            if ( same(*lat, breakvalue_) || same(*lon, breakvalue_) ) {
                push_back(new UserPoint(0,0,0,true));
                num_polylines++;
                val++;

                // in the case where we don't have enough values,
                // we will repeat the last one

                if (val == values_.end()) {
                    val--;
                }
            }
            else 
                push_back(new UserPoint(*lon, *lat,*val));
            lon++;
            lat++;

        }
    }
    
    // check for inconsistencies in numbers of points / values
    
    

}
