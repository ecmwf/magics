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


/*! 
    \brief Implementation of the Template class SpotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/
 


#include "GeoJSon.h"



using namespace magics;
using namespace json_spirit;



GeoJSon::GeoJSon() 
{
	
}

GeoJSon::~GeoJSon()
{
	
}

void GeoJSon::print(ostream& out) const
{
    out <<"ObsJSon[";
    GeoJSonAttributes::print(out);
	out << "]"; 
}

void GeoJSon::decode()
{
/*
	points_.clear();

	json_spirit::Value value;
    if ( !values_.empty() ) {
        for (vector<string>::iterator val = values_.begin(); val != values_.end(); ++val) {
		  json_spirit::read_or_throw(*val, value );
		  Object object = value.get_value< Object >();
		  points_.push_back(decode(object));
       }
        return; 
    }

	try {
	    ifstream is( path_.c_str());
	    json_spirit::Value value;

		  json_spirit::read_or_throw(is, value );
		  Object object = value.get_value< Object >();

		  for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {

			  Array points = entry->value_.get_value< Array>();

			  for (unsigned int i = 0; i < points.size(); i++) {
				  Object point = points[i].get_value< Object >();
				  points_.push_back(decode(point));
			  }
		  }
	 }
	 catch (std::exception e)
	 {
		 MagLog::error() << "Could not processed the file: " << path_ << ": " << e.what() << endl;
	 }
*/
}



void GeoJSon::points(const Transformation& transformation, vector<UserPoint>& points)
{
//	decode();

}

PointsHandler& GeoJSon::points(const Transformation& transformation, bool)
{
	decode();
   //return *pointsHandlers_.back();
}

void GeoJSon::customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool )
{

}
