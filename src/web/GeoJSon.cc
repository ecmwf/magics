/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
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
		GeoObject():parent_(0), shift_(false) {


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
		virtual void boundingBox(double& min, double& max) {
			for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
				(*object)->boundingBox(min, max);
			}
		}
		virtual void create(const std::set<string>& needs, CustomisedPointsList& out) {
			for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
				(*object)->create(needs, out);
			}
		}


		virtual bool shift() {
			for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
							if ( (*object)->shift() ) {
								shift_ = true;
								break;
							}
						}
			return shift_;
		}
		virtual void shift(PointsList& out) {

			if ( !shift() )
				return;


			for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
				(*object)->shift(out);
			}

		}
		virtual void shift(const std::set<string>& needs, CustomisedPointsList& out, double val = 0) {
					
					for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
						(*object)->shift(needs, out, val);
					}
		}
		vector<GeoObject*> objects_;
		virtual GeoObject* push_back(GeoObject* o) {

			objects_.push_back(o);

			o->parent_ = this;
			return o;
		}
		void newline(CustomisedPointsList& out) {
			if ( out.empty() )
				return;
			CustomisedPoint* last = out.back();
			CustomisedPoint* point = new CustomisedPoint();
			point->copyProperties(*last);
			point->missing(true);
			out.push_back(point);
		}
		bool shift_;
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

	void boundingBox(double& min, double& max) {
		min = 900000;
		max = -min;
		GeoObject::boundingBox(min, max);
		
	}
	void create(PointsList& out) {
		
		GeoObject::create(out);
		out.push_back(new UserPoint(0,0,0,true));
	}
	void shift(PointsList& out) {
			GeoObject::shift(out);
			out.push_back(new UserPoint(0,0,0,true));
	}
	void create(const std::set<string>& needs, CustomisedPointsList& out) {
		GeoObject::create(needs, out);
		newline(out);

	}
	void shift(const std::set<string>& needs, CustomisedPointsList& out, double val =0) {
			double min;
			double max;
			boundingBox(min, max);
			
			if (min < -180 ) 
				GeoObject::shift(needs, out, 360);
			if (min < 180 && max > 180) 
				GeoObject::shift(needs, out, -360);
			if (max > 360 ) 
				GeoObject::shift(needs, out, -360);
			newline(out);
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
	void boundingBox(double& min, double& max) {
		if ( min > lon_ ) min = lon_;
		if ( max < lon_ ) max = lon_;
	}
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
	void shift(PointsList& out, double value) {
		UserPoint* point = new UserPoint(lon_+value, lat_, tonumber(getProperty("value", "0")), false, false, getProperty("name"));

		out.push_back(point);
	}

	void set(const std::set<string>& needs, CustomisedPoint& point) {
		for (  std::set<string>::iterator need = needs.begin(); need != needs.end(); ++need ) {
			string value = getProperty(*need);
			if ( value.empty() )
				continue;
			point.insert(make_pair(*need, tonumber(value)));
		}
	}

	void create(const std::set<string>& needs, CustomisedPointsList& out) {
		CustomisedPoint* point = new CustomisedPoint(lon_, lat_,  getProperty("name"));
		set(needs, *point);
		out.push_back(point);
	}
	void shift(const std::set<string>& needs, CustomisedPointsList& out, double val =0) {
		CustomisedPoint* point = new CustomisedPoint(lon_+val, lat_,  getProperty("name"));
		set(needs, *point);
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
	void shift(PointsList& out) {
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
	if ( parent_ ) {
		parent_->create(*this);
		parent_->shift(*this);
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

void GeoJSon::customisedPoints(const Transformation&, const std::set<string>& needs, CustomisedPointsList& out, bool )
{
	decode();

	if ( parent_ ) {
			parent_->create(needs, out);
			parent_->shift(needs, out);
	}



}
