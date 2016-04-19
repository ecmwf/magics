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
#include "Factory.h"
#include "BinningObject.h"


using namespace magics;
using namespace json_spirit;

namespace magics {

class GeoObject
{
public:
		static int index_;
		GeoObject():parent_(0) {


			ostringstream n;
				n << "GeoObject_" << index_;
				name_ = n.str();

			index_++;

		}
		virtual ~GeoObject() {}
		virtual void decode(const json_spirit::Value& value) {}
		void properties(const json_spirit::Value& value) {
			Object object = value.get_value< Object >();
			for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {


				properties_.insert(make_pair(entry->name_, entry->value_.get_value<string>()));
			}
		}
		string name_;
		void properties(CustomisedPoint&) {}
		const string& getProperty(const string& name, const string& def = "") {


			map<string, string>::iterator property = properties_.find(name);
			if ( property != properties_.end() ) {

				return property->second;
			}
			if ( parent_ )
				return parent_->getProperty(name, def);
			return def;
		}

		map<string, string> properties_;
		GeoObject* parent_;
		virtual void create(PointsList& out) {

			for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {

				(*object)->create(out);
			}

		}
		vector<GeoObject*> objects_;
		virtual GeoObject* push_back(GeoObject* o) {

			objects_.push_back(o); o->parent_ = this; return o;
		}
};


int GeoObject::index_ = 0;
class GeoFeature : public GeoObject
{
public:
	GeoFeature() {
		ostringstream n;
		n << "GeoFeature_" << index_;
		name_ = n.str();

	}
	virtual ~GeoFeature() {}


	void create(PointsList& out) {
		GeoObject::create(out);
		out.push_back(new UserPoint(0,0,0,true));
	}
};

class GeoPoint : public GeoObject
{
public:
	GeoPoint() {
		ostringstream n;
		n << "GeoPoint_" << index_;
		name_ = n.str();

	}
	virtual ~GeoPoint() {}
	virtual void decode(const json_spirit::Value& value) {

		Array point = value.get_value< Array>();
		lon_ = point[0].get_value<double>();
		lat_ = point[1].get_value<double>();

	}
	double lat_;
	double lon_;
	void create(PointsList& out) {

		UserPoint* point = new UserPoint(lon_, lat_, tonumber(getProperty("value", "0")), false, false, getProperty("name"));
		out.push_back(point);

	}
};

class MultiLineString : public GeoObject
{
public:
	MultiLineString() {
		ostringstream n;
		n << "GeoPoint_" << index_;
		name_ = n.str();

	}
	virtual ~MultiLineString() {}
	virtual void decode(const json_spirit::Value& value) {

		Array lines = value.get_value< Array>();
		for (unsigned int i = 0; i < lines.size(); i++) {
			lines_.push_back(vector<pair<double, double> >());
			Array line = lines[i].get_value< Array >();
			for (unsigned int pt = 0; pt < line.size(); pt++) {
				Array point = line[pt].get_value< Array >();
				lines_.back().push_back(make_pair(point[0].get_value<double>(), point[1].get_value<double>()) );
			}
		}

	}
	vector<vector<pair<double, double> > > lines_;
	void create(PointsList& out) {
		double value = tonumber(getProperty("value", "0"));
		string name =  getProperty("name");
		for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
			for ( vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point ) {
				UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);
				out.push_back(upoint);
			}
			out.push_back(new UserPoint(0,0,0,true));
		}
	}

};

}
static SimpleObjectMaker<GeoPoint, GeoObject> Point("Point");
static SimpleObjectMaker<GeoFeature, GeoObject> FeatureCollection("FeatureCollection");
static SimpleObjectMaker<GeoObject> Feature("Feature");
static SimpleObjectMaker<MultiLineString, GeoObject> MultiLineString("MultiLineString");

GeoJSon::GeoJSon(): current_(0), parent_(0), matrix_(0)
{
	methods_["coordinates"] = &GeoJSon::coordinates;
	methods_["type"] = &GeoJSon::type;
	methods_["properties"] = &GeoJSon::properties;
	methods_["geometry"] = &GeoJSon::geometry;
	methods_["features"] = &GeoJSon::features;

}

GeoJSon::~GeoJSon()
{
	
}

MatrixHandler& GeoJSon::matrix()
{
	if  ( !matrix_ ) {
		decode();
		BinningObject binning;
		binning.x_ = "interval";
		binning.x_interval_ = binning_resolution_;
		binning.x_reference_ = -180.;
		binning.y_ = "interval";
		binning.y_interval_ = binning_resolution_;
		binning.y_reference_ = -90.;

		matrix_ = binning(*this);

	}

	matrixHandlers_.push_back(new MatrixHandler(*matrix_));
	return *(matrixHandlers_.back());
}

void GeoJSon::print(ostream& out) const
{
    out <<"ObsJSon[";
    GeoJSonAttributes::print(out);
	out << "]"; 
}
void GeoJSon::coordinates(const json_spirit::Value& value)
{
	ASSERT(current_);
	current_->decode(value);
}
void GeoJSon::properties(const json_spirit::Value& value)
{
	ASSERT(current_);
	current_->properties(value);
}
void GeoJSon::features(const json_spirit::Value& value)
{
	Array features = value.get_value< Array>();
	for (unsigned int i = 0; i < features.size(); i++) {
		dig(features[i]);
	}
}
void GeoJSon::geometry(const json_spirit::Value& value)
{
	dig(value);
}

string GeoJSon::find(json_spirit::Object& object, const string& name)
{
	for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {

		if ( entry->name_ == name )
			return entry->value_.get_value<string>();
	}
	return "";
}

void GeoJSon::dig(const json_spirit::Value& value)
{
	Object object = value.get_value< Object >();

	// Find the type :
	string type = find(object, "type" );

	GeoObject *previous = current_;
	if ( type != "" ) {
		GeoObject* current = SimpleObjectMaker<GeoObject>::create(type);
		previous = current_;
		current_ = ( current_ ) ? current_->push_back(current) : current;

		if ( !parent_ )
			parent_ = current_;
	}
	for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		map<string,  Method >::iterator method = methods_.find(entry->name_);

		if ( method != methods_.end() ) {
			( (this->*method->second)(entry->value_) );
		}
	}
	current_ = previous;
}

void GeoJSon::type(const json_spirit::Value& value)
{

}

void GeoJSon::decode()
{
	if ( points_.size() )
		return;
	points_.clear();

	try {
		json_spirit::Value value;
		if ( magCompare(type_, "string" ) ) {
			istringstream is(input_);
			json_spirit::read_or_throw(is, value );
		}
		else {
			ifstream is(path_.c_str());
			json_spirit::read_or_throw(is, value );
		}
		dig(value);
	}
	catch (std::exception e) {
		MagLog::error() << "Could not processed the file: " << path_ << ": " << e.what() << endl;
		abort();
	}
	if ( parent_ )
		parent_->create(*this);

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

void GeoJSon::customisedPoints(const Transformation&, const std::set<string>&, CustomisedPointsList& out, bool )
{
	//if ( current_ )
	//	current_->create(out);

}
