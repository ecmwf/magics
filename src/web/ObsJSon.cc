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
#include "Value.h"
#include "JSONParser.h"

using namespace magics;


ObsJSon::ObsJSon() {
    methods_["latitude"]   = &ObsJSon::latitude;
    methods_["longitude"]  = &ObsJSon::longitude;
    methods_["type"]       = &ObsJSon::type;
    methods_["identifier"] = &ObsJSon::identifier;
}

ObsJSon::~ObsJSon() {}


CustomisedPoint* ObsJSon::decode(ValueMap& point) {

    CustomisedPoint* current = new CustomisedPoint();
    for (auto key = point.begin(); key != point.end(); ++key) {
        map<string, ObsJSon::Method>::iterator method = methods_.find(key->first);
        
        if (method != methods_.end()) {
            (this->*method->second)(key->second, *current);
        }
        else {
            
            if (key->second.isDouble()) {
               
                (*current)[key->first] = key->second.get_value<double>();
            }
        }
    }

    return current;
}


void ObsJSon::decode() {
     
    points_.clear();
    Value value;
    if (!values_.empty()) {
        for (vector<string>::iterator val = values_.begin(); val != values_.end(); ++val) {
            ValueMap object = value.get_value<ValueMap>();
            points_.push_back(decode(object));
        }
        return;
    }

    try {
       
        
        Value value = JSONParser::decodeFile(path_);

        

        ValueMap object = value.get_value<ValueMap>();

        for (auto entry = object.begin(); entry != object.end(); ++entry) {
            
            ValueList points = entry->second.get_value<ValueList>();

            for (unsigned int i = 0; i < points.size(); i++) {
                ValueMap point = points[i].get_value<ValueMap>();
                points_.push_back(decode(point));
            }
        }
    }
    catch (std::exception& e) {
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

void ObsJSon::latitude(const Value& value, CustomisedPoint& point) {
    ASSERT(value.isDouble());
    point.latitude(value.get_value<double>());
}

void ObsJSon::longitude(const Value& value, CustomisedPoint& point) {
    ASSERT(value.isDouble());
    point.longitude(value.get_value<double>());
}

void ObsJSon::type(const Value& value, CustomisedPoint& point) {
    string type = value.get_value<string>();
    point.type(type);
    types_.insert(type);
}

void ObsJSon::identifier(const Value& value, CustomisedPoint& point) {
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
