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
	methods_["coordinates"] = &GeoJSon::coordinates;
	methods_["type"] = &GeoJSon::type;
	methods_["properties"] = &GeoJSon::properties;
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
void GeoJSon::coordinates(const json_spirit::Value& value)
{
	Array lines = value.get_value< Array>();
	for (unsigned int i = 0; i < lines.size(); i++) {
		Array line = lines[i].get_value< Array >();
		for (unsigned int pt = 0; pt < line.size(); pt++) {
			Array point = line[pt].get_value< Array >();
			push_back(new UserPoint(point[0].get_value<double>(), point[1].get_value<double>()));
		}
		push_back(new UserPoint(0,0,0,true));
	}
}
void GeoJSon::properties(const json_spirit::Value&)
{

}

void GeoJSon::type(const json_spirit::Value&)
{

}

void GeoJSon::decode()
{
	points_.clear();

	try {
		ifstream is( path_.c_str());
		json_spirit::Value value;
		json_spirit::read_or_throw(is, value );
		Object object = value.get_value< Object >();



		for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
			map<string,  Method >::iterator method = methods_.find(entry->name_);
			if ( method != methods_.end() ) {
				( (this->*method->second)(entry->value_) );
			}

		}
	}
	catch (std::exception e) {
		MagLog::error() << "Could not processed the file: " << path_ << ": " << e.what() << endl;
		abort();
	}

}



void GeoJSon::points(const Transformation& transformation, vector<UserPoint>& points)
{
	decode();

}

PointsHandler& GeoJSon::points(const Transformation& transformation, bool)
{
	decode();
	pointsHandlers_.push_back(new PointsHandler(*this));
	return *(pointsHandlers_.back());

}

void GeoJSon::customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList&, bool )
{

}
