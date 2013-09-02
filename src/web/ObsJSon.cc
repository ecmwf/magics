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


/*! \file SpotDecoder.h
    \brief Implementation of the Template class SpotDecoder.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/
 

#include <locale>
#include "ObsJSon.h"
#include "CustomisedPoint.h"
#include "json_spirit.h"
#include "MetaData.h"

using namespace magics;
using namespace json_spirit;


ObsJSon::ObsJSon() 
{
	methods_["latitude"] = &ObsJSon::latitude;
	methods_["longitude"] = &ObsJSon::longitude;
	methods_["type"] = &ObsJSon::type;
	methods_["identifier"] = &ObsJSon::identifier;
}

ObsJSon::~ObsJSon()
{
}

void ObsJSon::decode()
{
	points_.clear();

	ifstream is( path_.c_str());

	json_spirit::Value value;
	try {
		  json_spirit::read_or_throw(is, value );
		  Object object = value.get_value< Object >();

		  for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {

			  Array points = entry->value_.get_value< Array>();

			  for (unsigned int i = 0; i < points.size(); i++) {

				  points_.push_back(new CustomisedPoint());
				  CustomisedPoint& current = *points_.back();
				  Object point = points[i].get_value< Object >();
				  for (vector<Pair>::const_iterator key = point.begin(); key !=  point.end(); ++key) {

					  map<string,  Method >::iterator method = methods_.find(key->name_);
					  if ( method != methods_.end() ) {
					  	     (this->*method->second)(key->value_, current);
					  }
					  else {
						  if ( key->value_.type() == real_type )
							  current[key->name_] = key->value_.get_value< double>();
						  if ( key->value_.type() == int_type )
								  current[key->name_] = key->value_.get_value<int>();
					  }
				  }
			  }
		  }
	 }
	 catch (std::exception e)
	 {
		 MagLog::error() << "Could not processed the file: " << path_ << ": " << e.what() << endl;
		 abort();
	 }
}

void ObsJSon::print(ostream& out) const
{	
	out <<"ObsJSon[";
	ObsJSonAttributes::print(out);
	out << "]";
}

void ObsJSon::customisedPoints(const std::set<string>&, CustomisedPointsList& out)
{
	MagLog::dev() << "WrepJSon::customisedPoints" << *this <<endl;
	
	CustomisedPoint *point = new CustomisedPoint();

	point->latitude(50.);
	point->longitude(2.);

	(*point)["temperature"] = 280.;

	out.push_back(point);
}

void ObsJSon::customisedPoints(const Transformation&, const std::set<string>& what, CustomisedPointsList& out)
{

	decode();

	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point) {
			out.push_back(*point);

	}
}
void  ObsJSon::customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all)
{
	decode();

		for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point) {
			if ( t.in((*point)->longitude(), (*point)->latitude()) )
				out.push_back(*point);

		}
}

void ObsJSon::getInfo(const std::set<string>& what, multimap<string, string>& info)
{

	decode();
	for (std::set<string>::const_iterator t = types_.begin(); t !=types_.end(); ++t) {

			info.insert(make_pair("type", *t));
	}
}

void ObsJSon::latitude(const json_spirit::Value& value, CustomisedPoint& point)
{
	assert( value.type() == real_type);
	point.latitude(value.get_value< double>());
}

void ObsJSon::longitude(const json_spirit::Value& value, CustomisedPoint& point)
{
	assert( value.type() == real_type);
	point.longitude(value.get_value< double>());
}

void ObsJSon::type(const json_spirit::Value& value, CustomisedPoint& point)
{
	assert( value.type() == str_type);
	string type=value.get_value<string>();
	point.type(type);
	types_.insert(type);
}

void ObsJSon::identifier(const json_spirit::Value& value, CustomisedPoint& point)
{
	assert( value.type() == str_type);
	string type=value.get_value<string>();
	point.identifier(type);
}

void ObsJSon::visit(MetaDataVisitor& visitor)
{

	visitor.add("data", "{ \"objjson\": \"xxx\" }\n");

	decode();
	int i = 0;
	for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point) {

		visitor.add("point_" + tostring(i), (*point)->identifier());
		i++;
	}
}
