/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "ObsJSon.h"
#include <locale>
#include "CustomisedPoint.h"
#include "MetaData.h"
#include "json_spirit.h"

using namespace magics;
using namespace json_spirit;


ObsJSon::ObsJSon() {
    methods_["latitude"]   = &ObsJSon::latitude;
    methods_["longitude"]  = &ObsJSon::longitude;
    methods_["type"]       = &ObsJSon::type;
    methods_["identifier"] = &ObsJSon::identifier;
}

ObsJSon::~ObsJSon() {}


CustomisedPoint* ObsJSon::decode(Object& point) {
    CustomisedPoint* current = new CustomisedPoint();
    for (vector<Pair>::const_iterator key = point.begin(); key != point.end(); ++key) {
        map<string, ObsJSon::Method>::iterator method = methods_.find(key->name_);
        if (method != methods_.end()) {
            (this->*method->second)(key->value_, *current);
        }
        else {
            if (key->value_.type() == real_type)
                (*current)[key->name_] = key->value_.get_value<double>();
            if (key->value_.type() == int_type)
                (*current)[key->name_] = key->value_.get_value<int>();
        }
    }

    return current;
}


void ObsJSon::decode() {
    points_.clear();
    json_spirit::Value value;
    if (!values_.empty()) {
        for (vector<string>::iterator val = values_.begin(); val != values_.end(); ++val) {
            json_spirit::read_or_throw(*val, value);
            Object object = value.get_value<Object>();
            points_.push_back(decode(object));
        }
        return;
    }

    try {
        ifstream is(path_.c_str());
        json_spirit::Value value;

        json_spirit::read_or_throw(is, value);
        Object object = value.get_value<Object>();

        for (vector<Pair>::const_iterator entry = object.begin(); entry != object.end(); ++entry) {
            Array points = entry->value_.get_value<Array>();

            for (unsigned int i = 0; i < points.size(); i++) {
                Object point = points[i].get_value<Object>();
                points_.push_back(decode(point));
            }
        }
    }
    catch (std::exception e) {
        MagLog::error() << "Could not processed the file: " << path_ << ": " << e.what() << endl;
    }
}

void ObsJSon::print(ostream& out) const {
    out << "ObsJSon[";
    ObsJSonAttributes::print(out);
    out << "]";
}

void ObsJSon::customisedPoints(const std::set<string>&, CustomisedPointsList& out) {
    MagLog::dev() << "WrepJSon::customisedPoints" << *this << std::endl;

    CustomisedPoint* point = new CustomisedPoint();

    point->latitude(50.);
    point->longitude(2.);

    (*point)["temperature"] = 280.;

    out.push_back(point);
}

void ObsJSon::customisedPoints(const Transformation&, const std::set<string>& what, CustomisedPointsList& out) {
    decode();

    for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point) {
        out.push_back(*point);
    }
}
void ObsJSon::customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out,
                               bool all) {
    decode();

    for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point) {
        if (t.in((*point)->longitude(), (*point)->latitude()))
            out.push_back(*point);
    }
}

void ObsJSon::getInfo(const std::set<string>& what, multimap<string, string>& info) {
    decode();
    for (std::set<string>::const_iterator t = types_.begin(); t != types_.end(); ++t) {
        info.insert(make_pair("type", *t));
    }
}

void ObsJSon::latitude(const json_spirit::Value& value, CustomisedPoint& point) {
    ASSERT(value.type() == real_type);
    point.latitude(value.get_value<double>());
}

void ObsJSon::longitude(const json_spirit::Value& value, CustomisedPoint& point) {
    ASSERT(value.type() == real_type);
    point.longitude(value.get_value<double>());
}

void ObsJSon::type(const json_spirit::Value& value, CustomisedPoint& point) {
    ASSERT(value.type() == str_type);
    string type = value.get_value<string>();
    point.type(type);
    types_.insert(type);
}

void ObsJSon::identifier(const json_spirit::Value& value, CustomisedPoint& point) {
    ASSERT(value.type() == str_type);
    string type = value.get_value<string>();
    point.identifier(type);
}

void ObsJSon::visit(MetaDataVisitor& visitor) {
    visitor.add("data", "{ \"objjson\": \"xxx\" }\n");

    decode();
    int i = 0;
    for (vector<CustomisedPoint*>::const_iterator point = points_.begin(); point != points_.end(); ++point) {
        visitor.add("point_" + tostring(i), (*point)->identifier());
        i++;
    }
}
