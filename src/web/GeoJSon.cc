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
#include "BinningObject.h"
#include "Factory.h"
#include "Value.h"
#include "JSONParser.h"

using namespace magics;

namespace magics {

class GeoObject {
public:
    static int index_;
    GeoObject() : parent_(0), shift_(false) {
        ostringstream n;
        n << "GeoObject_" << index_;
        name_ = n.str();

        index_++;
    }
    virtual ~GeoObject() {}
    string convert(const Value&);
    virtual void decode(const Value& value) {}
    void properties(const Value& value) {
        ValueMap object = value.get_value<ValueMap>();
        for (auto entry = object.begin(); entry != object.end(); ++entry) {
            properties_.insert(make_pair(entry->first, convert(entry->second)));
        }
    }
    string name_;
    void properties(CustomisedPoint&) {}
    const string& getProperty(const string& name, const string& def = "") {
        map<string, string>::iterator property = properties_.find(name);
        if (property != properties_.end()) {
            return property->second;
        }
        if (parent_)
            return parent_->getProperty(name, def);
        return def;
    }

    map<string, string> properties_;
    GeoObject* parent_;
    virtual void create(PointsList& out, const string& ref) {
        for (vector<GeoObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
            (*object)->create(out, getProperty("reference_longitude", ref));
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
            if ((*object)->shift()) {
                shift_ = true;
                break;
            }
        }
        return shift_;
    }
    virtual void shift(PointsList& out) {
        if (!shift())
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
        if (out.empty())
            return;
        CustomisedPoint* last  = out.back().get();
        CustomisedPoint* point = new CustomisedPoint();
        point->copyProperties(*last);
        point->missing(true);
        out.push_back(point);
    }
    bool shift_;
    virtual void print() {
        for (vector<GeoObject*>::iterator o = objects_.begin(); o != objects_.end(); ++o)
            (*o)->print();
    }
    virtual bool detectFeature() {
        for (vector<GeoObject*>::iterator o = objects_.begin(); o != objects_.end(); ++o)
            if ((*o)->detectFeature())
                return true;
        return false;
    }
};


int GeoObject::index_ = 0;
class GeoFeature : public GeoObject {
public:
    GeoFeature() {
        ostringstream n;
        n << "GeoFeature_" << index_;
        name_ = n.str();
    }

    bool detectFeature() { return true; }

    virtual ~GeoFeature() {}

    void boundingBox(double& min, double& max) {
        min = 900000;
        max = -min;
        GeoObject::boundingBox(min, max);
    }
    void create(PointsList& out, const string& ref) {
        GeoObject::create(out, ref);
        out.push_back(new UserPoint(0, 0, 0, true));
    }
    void shift(PointsList& out) {
        GeoObject::shift(out);
        out.push_back(new UserPoint(0, 0, 0, true));
    }
    void create(const std::set<string>& needs, CustomisedPointsList& out) {
        GeoObject::create(needs, out);
        newline(out);
    }
    void shift(const std::set<string>& needs, CustomisedPointsList& out, double val = 0) {
        double min;
        double max;
        if (GeoObject::detectFeature())  // Dig!
            return GeoObject::shift(needs, out);

        boundingBox(min, max);

        if (max <= -180)
            GeoObject::shift(needs, out, 360);

        else if (min >= 180)
            GeoObject::shift(needs, out, -360);

        else if (min <= 180 && max >= 180) {
            GeoObject::shift(needs, out, -360);
        }
        else if (min <= -180 && max >= -180) {
            GeoObject::shift(needs, out, 360);
        }
        else if (max > 360)
            GeoObject::shift(needs, out, -360);

        newline(out);
    }
};

class GeoPoint : public GeoObject {
public:
    GeoPoint() {
        ostringstream n;
        n << "GeoPoint_" << index_;
        name_ = n.str();
    }
    virtual ~GeoPoint() {}
    void boundingBox(double& min, double& max) {
        if (min > lon_)
            min = lon_;
        if (max < lon_)
            max = lon_;
    }
    virtual void decode(const Value& value) {
        ValueList point = value.get_value<ValueList>();
        lon_        = point[0].get_value<double>();
        lat_        = point[1].get_value<double>();
    }
    void print() {}
    double lat_;
    double lon_;

    void create(PointsList& out, const string& ref) {
        double reference = tonumber(ref);
        double min       = reference - 180;
        double max       = reference + 180;
        if (reference != -9999) {
            while (lon_ < min)
                lon_ += 360;
            while (lon_ > max)
                lon_ -= 360;
        }


        UserPoint* point =
            new UserPoint(lon_, lat_, tonumber(getProperty("value", "0")), false, false, getProperty("name"));

        out.push_back(point);
    }
    void shift(PointsList& out, double value) {
        UserPoint* point =
            new UserPoint(lon_ + value, lat_, tonumber(getProperty("value", "0")), false, false, getProperty("name"));

        out.push_back(point);
    }

    void set(const std::set<string>& needs, CustomisedPoint& point) {
        for (std::set<string>::iterator need = needs.begin(); need != needs.end(); ++need) {
            string value = getProperty(*need);
            if (value.empty())
                continue;
            point.insert(make_pair(*need, tonumber(value)));
        }
    }

    void create(const std::set<string>& needs, CustomisedPointsList& out) {
        CustomisedPoint* point = new CustomisedPoint(lon_, lat_, getProperty("name"));
        set(needs, *point);
        out.push_back(point);
    }
    void shift(const std::set<string>& needs, CustomisedPointsList& out, double val = 0) {
        CustomisedPoint* point = new CustomisedPoint(lon_ + val, lat_, getProperty("name"));
        set(needs, *point);
        out.push_back(point);
    }
};

class MultiLineString : public GeoObject {
public:
    MultiLineString() {
        ostringstream n;
        n << "GeoPoint_" << index_;
        name_ = n.str();
    }
    virtual ~MultiLineString() {}
    virtual void decode(const Value& value) {
        ValueList lines = value.get_value<ValueList>();
        for (unsigned int i = 0; i < lines.size(); i++) {
            lines_.push_back(vector<pair<double, double> >());
            ValueList line = lines[i].get_value<ValueList>();
            for (unsigned int pt = 0; pt < line.size(); pt++) {
                ValueList point = line[pt].get_value<ValueList>();
                lines_.back().push_back(make_pair(point[0].get_value<double>(), point[1].get_value<double>()));
            }
        }
    }
    vector<vector<pair<double, double> > > lines_;
    void create(PointsList& out, const string& ref) {
        double value = tonumber(getProperty("value", "0"));
        string name  = getProperty("name");
        for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
            for (vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point) {
                UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);
                out.push_back(upoint);
            }
            out.push_back(new UserPoint(0, 0, 0, true));
        }
    }
    void shift(PointsList& out) {
        double value = tonumber(getProperty("value", "0"));
        string name  = getProperty("name");
        for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
            for (vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point) {
                UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);
                out.push_back(upoint);
            }
            out.push_back(new UserPoint(0, 0, 0, true));
        }
    }
};

class MultiPolygon : public GeoObject {
public:
    MultiPolygon() {
        ostringstream n;
        n << "GeoPoint_" << index_;
        name_ = n.str();
    }
    virtual ~MultiPolygon() {}
    virtual void decode(const Value& value) {
        ValueList multi = value.get_value<ValueList>();

        for (unsigned int m = 0; m < multi.size(); m++) {
            ValueList alines = multi[m].get_value<ValueList>();

            for (unsigned int i = 0; i < alines.size(); i++) {
                ValueList line = alines[i].get_value<ValueList>();
                lines_.push_back(vector<pair<double, double> >());
                lines_.back().reserve(line.size());

                for (unsigned int pt = 0; pt < line.size(); pt++) {
                    ValueList point = line[pt].get_value<ValueList>();
                    lines_.back().push_back(make_pair(point[0].get_value<double>(), point[1].get_value<double>()));
                }
            }
        }
    }
    vector<vector<pair<double, double> > > lines_;
    void create(PointsList& out, const string& ref) {
        double value = tonumber(getProperty("value", "0"));
        string name  = getProperty("name");
        for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
            for (vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point) {
                UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);

                out.push_back(upoint);
            }
            out.push_back(new UserPoint(0, 0, 0, true));
        }
    }
    void shift(PointsList& out) {
        // double value = tonumber(getProperty("value", "0"));
        // string name  = getProperty("name");
        // for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
        //     for (vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point) {
        //         UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);
        //         out.push_back(upoint);
        //     }
        //     out.push_back(new UserPoint(0, 0, 0, true));
        // }
    }
};
class MagPolygon : public GeoObject {
public:
    MagPolygon() {
        ostringstream n;
        n << "GeoPoint_" << index_;
        name_ = n.str();
    }
    virtual ~MagPolygon() {}
    virtual void decode(const Value& value) {
        ValueList alines = value.get_value<ValueList>();
        // WE take the first one for the first trial!
        // The next one are the holes !

        for (unsigned int i = 0; i < alines.size(); i++) {
            ValueList line = alines[i].get_value<ValueList>();

            lines_.push_back(vector<pair<double, double> >());
            lines_.back().reserve(line.size());
            for (unsigned int pt = 0; pt < line.size(); pt++) {
                ValueList point = line[pt].get_value<ValueList>();
                lines_.back().push_back(make_pair(point[0].get_value<double>(), point[1].get_value<double>()));
            }
        }
    }
    vector<vector<pair<double, double> > > lines_;
    void create(PointsList& out, const string& ref) {
        double value = tonumber(getProperty("value", "0"));
        string name  = getProperty("name");
        for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
            for (vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point) {
                UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);

                out.push_back(upoint);
            }
            out.push_back(new UserPoint(0, 0, 0, true));
        }
    }
    void shift(PointsList& out) {
        // double value = tonumber(getProperty("value", "0"));
        // string name  = getProperty("name");
        // for (vector<vector<pair<double, double> > >::iterator line = lines_.begin(); line != lines_.end(); ++line) {
        //     for (vector<pair<double, double> >::iterator point = line->begin(); point != line->end(); ++point) {
        //         UserPoint* upoint = new UserPoint(point->first, point->second, value, false, false, name);
        //         out.push_back(upoint);
        //     }
        //     out.push_back(new UserPoint(0, 0, 0, true));
        // }
    }
};  // namespace magics

}  // namespace magics
static SimpleObjectMaker<GeoPoint, GeoObject> Point("Point");
static SimpleObjectMaker<GeoFeature, GeoObject> FeatureCollection("FeatureCollection");
static SimpleObjectMaker<GeoObject> Feature("Feature");
static SimpleObjectMaker<MultiLineString, GeoObject> MultiLineString("MultiLineString");
static SimpleObjectMaker<MultiPolygon, GeoObject> MultiPolygon("MultiPolygon");
static SimpleObjectMaker<MagPolygon, GeoObject> MagPolygon("Polygon");


GeoJSon::GeoJSon() : current_(0), parent_(0), matrix_(0) {
    methods_["coordinates"] = &GeoJSon::coordinates;
    methods_["type"]        = &GeoJSon::type;
    methods_["properties"]  = &GeoJSon::properties;
    methods_["geometry"]    = &GeoJSon::geometry;
    methods_["features"]    = &GeoJSon::features;
}

GeoJSon::~GeoJSon() {}

string GeoObject::convert(const Value& value) {
    if(value.isString() || value.isNumber() || value.isDouble()) {
        std::string s(value);
        return s;
    }
    return "";
}

MatrixHandler& GeoJSon::matrix() {
    if (!matrix_) {
        decode();
        BinningObject binning;
        binning.x_           = "interval";
        binning.x_interval_  = binning_resolution_;
        binning.x_reference_ = -180.;
        binning.y_           = "interval";
        binning.y_interval_  = binning_resolution_;
        binning.y_reference_ = -90.;

        matrix_ = binning(*this);
    }

    matrixHandlers_.push_back(new MatrixHandler(*matrix_));
    return *(matrixHandlers_.back());
}

void GeoJSon::print(ostream& out) const {
    out << "ObsJSon[";
    GeoJSonAttributes::print(out);
    out << "]";
}
void GeoJSon::coordinates(const Value& value) {
    ASSERT(current_);
    current_->decode(value);
}
void GeoJSon::properties(const Value& value) {
    ASSERT(current_);
    current_->properties(value);
}
void GeoJSon::features(const Value& value) {
    ValueList features = value.get_value<ValueList>();
    for (unsigned int i = 0; i < features.size(); i++) {
        dig(features[i]);
    }
}
void GeoJSon::geometry(const Value& value) {
    dig(value);
}

string GeoJSon::find(ValueMap& object, const string& name) {
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        if (entry->first == name)
            return entry->second.get_value<string>();
    }
    return "";
}

void GeoJSon::dig(const Value& value) {
    ValueMap object = value.get_value<ValueMap>();

    // Find the type :
    string type = find(object, "type");

    GeoObject* previous = current_;
    if (type != "") {
        GeoObject* current = SimpleObjectMaker<GeoObject>::create(type);
        previous           = current_;
        current_           = (current_) ? current_->push_back(current) : current;

        if (!parent_)
            parent_ = current_;
    }
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        map<string, Method>::iterator method = methods_.find(entry->first);

        if (method != methods_.end()) {
            ((this->*method->second)(entry->second));
        }
    }
    current_ = previous;
}

void GeoJSon::type(const Value& value) {}

void GeoJSon::decode() {
    if (points_.size())
        return;
    points_.clear();

    try {
        Value value;
        if (magCompare(type_, "string")) {
           value = JSONParser::decodeString(input_);
        }
        else {
            try {
                ifstream is(path_.c_str());
                value = JSONParser::decodeFile(path_);
            }
            catch (std::exception& e) {
                MagLog::error() << "JSON error in file: " << path_ << ": " << e.what() << endl;
                return;
            }
            catch (...) {
                MagLog::error() << "GeoJSon decoder: can not read file " << path_ << endl;
                return;
            }
        }
        dig(value);
    }
    catch (std::exception e) {
        MagLog::error() << "Could not processed the file: " << path_ << ": " << e.what() << endl;
        abort();
    }
    if (parent_) {
        parent_->create(*this, "-9999");
        parent_->shift(*this);
    }
}


void GeoJSon::points(const Transformation& transformation, vector<UserPoint>& points) {
    decode();
}

PointsHandler& GeoJSon::points(const Transformation& transformation, bool) {
    decode();
    pointsHandlers_.push_back(new PointsHandler(*this));
    return *(pointsHandlers_.back());
}

void GeoJSon::customisedPoints(const Transformation&, const std::set<string>& needs, CustomisedPointsList& out, bool) {
    decode();


    if (parent_) {
        parent_->create(needs, out);
        parent_->shift(needs, out);
    }
}
