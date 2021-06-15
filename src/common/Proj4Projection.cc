/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file Proj4Projection.cc
    \brief Implementation of Proj4Projection.
    \author Meteorological Visualisation Section, ECMWF

    Started: Tue May 18 17:39:58 2010

*/

#include "Proj4Projection.h"
#include "GridPlotting.h"
#include "LabelPlotting.h"
#include "Layer.h"
#include "MagJSon.h"
#include "MatrixHandler.h"
#include "ParameterSettings.h"
#include "Polyline.h"
#include "Text.h"

#include <cmath>

#include "MagConfig.h"
#include "MetaData.h"
#include "Value.h"

namespace magics {

class Epsg {
public:
    Epsg(const string& name) : name_(name) {
        epsgs_.insert(make_pair(lowerCase(name), this));
        methods_["definition"]      = &Epsg::definition;
        methods_["min_longitude"]   = &Epsg::minlon;
        methods_["min_latitude"]    = &Epsg::minlat;
        methods_["max_longitude"]   = &Epsg::maxlon;
        methods_["max_latitude"]    = &Epsg::maxlat;
        methods_["method"]          = &Epsg::method;
        initMethods_["geos"]        = &Epsg::geosinit;
        initMethods_["tpers"]       = &Epsg::tpersinit;
        initMethods_["polar_north"] = &Epsg::polarinit;
        initMethods_["polar_south"] = &Epsg::polarsouthinit;
        initMethods_["EPSG:32661"]  = &Epsg::epsg32661;
        initMethods_["EPSG:32761"]  = &Epsg::epsg32761;    
    }
    string name_;
    string definition_;

    typedef void (Epsg::*InitMethod)(const Proj4Projection&);
    typedef void (Epsg::*Method)(const Value&);
    map<string, Method> methods_;
    map<string, InitMethod> initMethods_;

    void definition(const Value& value) { definition_ = value.get_value<string>(); }
    void minlon(const Value& value) { minlon_ = value.get_value<double>(); }
    void minlat(const Value& value) { minlat_ = value.get_value<double>(); }
    void maxlon(const Value& value) { maxlon_ = value.get_value<double>(); }
    void maxlat(const Value& value) { maxlat_ = value.get_value<double>(); }
    void method(const Value& value) { method_ = value.get_value<string>(); }

    void init(const Proj4Projection& from) {
        map<string, InitMethod>::iterator initmethod = initMethods_.find(name_);
        if (initmethod != initMethods_.end()) {
            (this->*initmethod->second)(from);
        }
    }

    void geosinit(const Proj4Projection& from) {
        minlon_ = from.vertical_longitude_ - 80;
        maxlon_ = from.vertical_longitude_ + 80;
        minlat_ = -80;
        maxlat_ = 80;

        ostringstream def;
        def << "+proj=geos +h=42164000 +ellps=WGS84 +lon_0=" << from.vertical_longitude_;

        // def << "++proj=tpers    +ellps=WGS84 +h=5000000  +lat_0=20 +lon_0=-60
        // +x_0=0 +y_0=0 +azi=20 +tilt=0 +units=m";
        definition_ = def.str();
    }

    void tpersinit(const Proj4Projection& from) {
        ostringstream def;

        def << "+proj=tpers +ellps=WGS84 +h=" << from.projection_height_;
        def << " +lat_0=" << from.projection_view_latitude_;
        def << " +lon_0=" << from.projection_view_longitude_;
        def << " +x_0=0 +y_0=0 +azi=" << from.projection_azimuth_;
        def << " +tilt=" << from.projection_tilt_ << "  +units=m";


        definition_ = def.str();
        // definition_ = "+proj=tpers +h=5500000 +lat_0=40";
    }

    void polarinit(const Proj4Projection& from) {
        ostringstream def;

        def << "+proj=stere +lat_0=90 ";
        def << " +lon_0=" << from.vertical_longitude_;
        def << " +k=0.994 +x_0=2000000 +y_0=2000000  "
               "+units=m";
        definition_ = def.str();
    }

    void polarsouthinit(const Proj4Projection& from) {
        ostringstream def;

        def << "+proj=stere +lat_0=-90 +lat_ts=" << from.true_scale_south_;
        def << " +lon_0=" << from.vertical_longitude_;
        def << "  +x_0=2000000 +y_0=2000000 +ellps=WGS84 +datum=WGS84 "
               "+units=m ";
        definition_ = def.str();
    }

      void epsg32661(const Proj4Projection& from) {
        ostringstream def;

        def << "+proj=stere +lat_0=90 +lat_ts=90 ";
        def << " +lon_0=" << from.vertical_longitude_;
        def << " +k=0.994 +x_0=2000000 +y_0=2000000  "
               " +datum=WGS84 +units=m";
        definition_ = def.str();
    }

    void epsg32761(const Proj4Projection& from) {
        ostringstream def;
        def << "+proj=stere +lat_0=-90 +lat_ts=-90 ";
        def << " +lon_0=" << from.vertical_longitude_;
        def << " +k=0.994 +x_0=2000000 +y_0=2000000  "
               " +datum=WGS84 +units=m";
        definition_ = def.str();
    }

    double minlon_;
    double minlat_;
    double maxlon_;
    double maxlat_;

    string method_;

    static map<string, Epsg*> epsgs_;

    void set(const Value&);

    static Epsg* find(const Proj4Projection& from) {
        string name                       = lowerCase(from.name());
        map<string, Epsg*>::iterator epsg = epsgs_.find(name);
        if (epsg == epsgs_.end()) {
            MagLog::warning() << "Cannot find information on " << name << ": use epsg instead" << endl;
            return epsgs_.find("EPSG:4326")->second;
        }
        epsg->second->init(from);

        return epsg->second;
    }

    const char* definition() { return definition_.c_str(); }
};

class EpsgConfig : public MagConfig {
public:
    EpsgConfig() {}
    ~EpsgConfig() {}

    void callback(const string&, const Value&);
    void init();
    Epsg* epsg_;
};
}  // namespace magics
using namespace magics;

map<string, Epsg*> Epsg::epsgs_;

void EpsgConfig::init() {
    // methods_["epsg"] =  &EpsgConfig::epsg;
    MagConfigHandler(buildSharePath("epsg.json"), *this);
}
void Epsg::set(const Value& value) {
    ValueMap object = value.get_value<ValueMap>();
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        map<string, Method>::iterator method = methods_.find(entry->first);
        if (method != methods_.end()) {
            ((this->*method->second)(entry->second));
        }
    }
}
void EpsgConfig::callback(const string& name, const Value& value) {
    // here we get an Array of epsg!

    ValueList values = value.get_value<ValueList>();
    for (unsigned int i = 0; i < values.size(); i++) {
        ValueMap object = values[i].get_value<ValueMap>();
        for (auto entry = object.begin(); entry != object.end(); ++entry) {
            Epsg* epsg = new Epsg(entry->first);
            epsg->set(entry->second);
        }
    }
}

/*!
  \brief Constructor
*/
Proj4Projection::Proj4Projection(const string& definition) :
    definition_(definition),
    gridMinLon_(DBL_MAX),
    gridMinLat_(DBL_MAX),
    gridMaxLon_(-DBL_MAX),
    gridMaxLat_(-DBL_MAX),
    wraparound_(false),
    helper_(0) {
    // init();
    EpsgConfig config;
    config.init();
    name_ = definition;
}

Proj4Projection::Proj4Projection() :
    gridMinLon_(DBL_MAX),
    gridMinLat_(DBL_MAX),
    gridMaxLon_(-DBL_MAX),
    gridMaxLat_(-DBL_MAX),
    wraparound_(false),
    helper_(0) {
    // init();
    EpsgConfig config;
    config.init();
}
void Proj4Projection::populate(double lon, double lat, double val, vector<UserPoint>& out) const {
    if (inExtended(PaperPoint(lon , lat)))
        out.push_back(UserPoint(lon, lat, val));
    if (inExtended(PaperPoint(lon - 360, lat)))
        out.push_back(UserPoint(lon - 360., lat, val));
    if (inExtended(PaperPoint(lon + 360, lat)))
        out.push_back(UserPoint(lon + 360., lat, val));
}

/*!
  \brief Destructor
*/
Proj4Projection::~Proj4Projection() {}

void Proj4Projection::print(ostream& out) const {
    out << "Proj4Projection[";
    Proj4ProjectionAttributes::print(out);
    out << "]";
}

magics::Polyline& Proj4Projection::getPCBoundingBox() const {
    return *PCEnveloppe_;
}

magics::Polyline& Proj4Projection::getUserBoundingBox() const {
    return *userEnveloppe_;
}

void Proj4Projection::init() {
    MagLog::dev() << "Proj4Projection::init()" << *this << endl;

    projection_ = Epsg::find(*this);
    helper_     = new LatLonProjP(projection_->definition());

    if (!helper_->valid()) {
        // MagLog::error() << pj_strerrno(pj_errno) << endl;
        MagLog::error() << " proj4 error " << projection_->definition() << endl;
        ASSERT(false);
    }

    methods_["geos"]   = &Proj4Projection::geos;
    methods_["tpers"]  = &Proj4Projection::tpers;
    methods_["polar"]  = &Proj4Projection::conic;
    methods_["conic"]  = &Proj4Projection::conic;
    methods_["simple"] = &Proj4Projection::simple;

    map<string, InitMethod>::iterator method = methods_.find(projection_->method_);
    if (method != methods_.end())
        (this->*method->second)();
    else
        simple();

    helpers_["full"]       = &Proj4Projection::full;
    helpers_["corners"]    = &Proj4Projection::corners;
    helpers_["centre"]     = &Proj4Projection::centre;
    helpers_["projection"] = &Proj4Projection::projectionSimple;

    if (coordinates_system_ == "projection")
        setting_ = "projection";

    map<string, SettingHelper>::iterator helper = helpers_.find(lowerCase(setting_));

    if (helper != helpers_.end())
        (this->*helper->second)();
    else {
        MagLog::warning() << " Could not find method " << setting_ << " to set the geographical area"
                          << "  Going back to default area" << endl;
        full();
    }

    xgutter_ = ((xpcmax_ - xpcmin_ ) * gutter_)/100;
    ygutter_ = ((ypcmax_ - ypcmin_ ) * gutter_)/100;

    askedxmin_ = std::min(xpcmin_, xpcmax_);
    askedxmax_ = std::max(xpcmin_, xpcmax_);
    askedymin_ = std::min(ypcmin_, ypcmax_);
    askedymax_ = std::max(ypcmin_, ypcmax_);
}

bool Proj4Projection::addSouth() const {
    return (projection_->method_ == "simple");
}
void Proj4Projection::full() {
    if (projection_->method_ == "simple") {
        if (max_longitude_ != 180.) {
            if (max_latitude_ == 90)
                max_latitude_ = projection_->maxlat_;
            if (min_latitude_ == -90)
                min_latitude_ = projection_->minlat_;
            corners();
            return;
        }
        if (max_longitude_ == 180.) {
            projection_->maxlon_ = 180.;
            if (max_latitude_ == 90)
                max_latitude_ = projection_->maxlat_;
            if (min_latitude_ == -90)
                min_latitude_ = projection_->minlat_;
            corners();
            return;
        }

        if (min_longitude_ != -180) {
            // set projection default !
            if (max_latitude_ == 90)
                max_latitude_ = projection_->maxlat_;
            if (min_latitude_ == -90)
                min_latitude_ = projection_->minlat_;
            corners();
        }

        if (min_latitude_ != -90) {
            if (max_latitude_ == 90)
                max_latitude_ = projection_->maxlat_;
            corners();
            return;
        }
        if (max_latitude_ != 90) {
            if (min_longitude_ == -180)
                min_longitude_ = projection_->minlon_;
            if (min_latitude_ == -90)
                min_latitude_ = projection_->minlat_;
            corners();
            return;
        }
    }
}

void Proj4Projection::wrap(double& x, double& y) {}

void Proj4Projection::corners() {
    // we have to update the PCBounding box!
    xpcmin_ = min_longitude_;
    ypcmin_ = min_latitude_;
    xpcmax_ = max_longitude_;
    ypcmax_ = max_latitude_;

    fast_reproject(xpcmin_, ypcmin_);
    fast_reproject(xpcmax_, ypcmax_);

    delete PCEnveloppe_;

    PCEnveloppe_ = new magics::Polyline();
    PCEnveloppe_->box(PaperPoint(xpcmin_, ypcmin_), PaperPoint(xpcmax_, ypcmax_));
}

void Proj4Projection::centre() {}

PaperPoint Proj4Projection::operator()(const UserPoint& point) const {
    if (!helper_) {
        projection_ = Epsg::find(*this);
        helper_     = new LatLonProjP(projection_->definition());

        if (!helper_->valid()) {
            // MagLog::error() << pj_strerrno(pj_errno) << endl;
            MagLog::error() << " proj4 error " << projection_->definition() << endl;
            ASSERT(false);
        }
    }
    double x = point.x();
    double y = point.y();

    int error = helper_->convert(x, y);
    if (error) {
        MagLog::debug() << helper_->error(error) << " for " << point << endl;
        //		NON Valeurs trop faibles en EPSG:3857 par exemple !  -->
        // Transformation::in(pp) == True !
        //		-> Avec ces valeurs, si grille globale, les barbules des poles
        // sont tracées dans l'atlantique sud 		en formant une couronne
        // centrée vers( 9°W, 66°35S) 		return PaperPoint(1000000,
        // 10000000);   .// En plus il manque 1 zero !
        return PaperPoint(HUGE_VAL,
                          HUGE_VAL);  // Devrait suffire pour que Transformation::in(pp) == False
                                      // qqsoient from_ et to_
    }

    return PaperPoint(x, y, point.value_, point.missing(), point.border(), 0, point.name());
}

PaperPoint Proj4Projection::operator()(const PaperPoint& point) const {
    MagLog::dev() << "Proj4Projection::operator()(...) needs implementing." << endl;
    return Transformation::operator()(point);
}

void Proj4Projection::setNewPCBox(double minx, double miny, double maxx, double maxy) {
    PaperPoint p1(minx, miny);
    PaperPoint p2(maxx, maxy);
    UserPoint ll, ur;

    revert(p1, ll);
    revert(p2, ur);

    min_longitude_ = ll.x();
    max_longitude_ = ur.x();
    min_latitude_  = ll.y();
    max_latitude_  = ur.y();
    xpcmin_       = minx;
    xpcmax_       = maxx;
    ypcmin_       = miny;
    ypcmax_       = maxy;
}

void Proj4Projection::revert(const PaperPoint& xy, UserPoint& point) const {
    static bool first = true;
    if (first) {
        const_cast<Proj4Projection*>(this)->init();
        first = false;
    }
    if (PCEnveloppe_->within(xy) == false) {
        point = UserPoint(HUGE_VAL, HUGE_VAL);

        return;
    }

    double x = xy.x();
    double y = xy.y();

    int error = helper_->revert(x, y);

    if (error) {
        MagLog::debug() << helper_->error(error) << endl;
        point = UserPoint(HUGE_VAL, HUGE_VAL);
        return;
    }

    point = UserPoint(x, y);
}

bool Proj4Projection::needShiftedCoastlines() const {
    // Will need w new parameter to know!
    return false;
}

void Proj4Projection::aspectRatio(double& width, double& height) {
    MagLog::dev() << "Proj4Projection::aspectRatio(...) needs implementing." << endl;
    Transformation::aspectRatio(width, height);
}

void Proj4Projection::add(double lon, double lat) {
    double x  = lon;
    double y  = lat;
    int error = helper_->convert(x, y);
    userEnveloppe_->push_back(PaperPoint(lon, lat));
    PCEnveloppe_->push_back(PaperPoint(x, y));

    if (x < xpcmin_)
        xpcmin_ = x;
    if (y < ypcmin_)
        ypcmin_ = y;
    if (x > xpcmax_)
        xpcmax_ = x;
    if (y > ypcmax_)
        ypcmax_ = y;

    if (lon < gridMinLon_)
        gridMinLon_ = lon;
    if (lat < gridMinLat_)
        gridMinLat_ = lat;
    if (lon > gridMaxLon_)
        gridMaxLon_ = lon;
    if (lat > gridMaxLat_)
        gridMaxLat_ = lat;
}

void Proj4Projection::conic() {
    userEnveloppe_->clear();
    PCEnveloppe_->clear();
    xpcmin_ = DBL_MAX;
    ypcmin_ = DBL_MAX;
    xpcmax_ = -DBL_MAX;
    ypcmax_ = -DBL_MAX;

    // top
    add(projection_->minlon_ - vertical_longitude_, projection_->maxlat_);
    for (float lon = projection_->minlon_; lon <= projection_->maxlon_; lon++) {
        add(lon, projection_->minlat_);
    }
    add(projection_->maxlon_, projection_->maxlat_);
}

void Proj4Projection::simple() {
    userEnveloppe_->clear();
    PCEnveloppe_->clear();
    xpcmin_ = DBL_MAX;
    ypcmin_ = DBL_MAX;
    xpcmax_ = -DBL_MAX;
    ypcmax_ = -DBL_MAX;

    add(projection_->minlon_, projection_->minlat_);
    add(projection_->minlon_, projection_->maxlat_);
    add(projection_->maxlon_, projection_->maxlat_);
    add(projection_->maxlon_, projection_->minlat_);
    add(projection_->minlon_, projection_->minlat_);

    width_ = projection_->maxlon_ - projection_->minlon_;

    double minx = projection_->minlon_;
    double y1, y2 = (projection_->maxlat_ + projection_->minlat_);
    double maxx = projection_->maxlon_;

    fast_reproject(minx, y1);
    fast_reproject(maxx, y1);
    pwidth_ = maxx - minx;

    wraparound_ = false;
}

void Proj4Projection::projectionSimple() {
    xpcmin_ = min_longitude_;
    ypcmin_ = min_latitude_;
    xpcmax_ = max_longitude_;
    ypcmax_ = max_latitude_;


    int error;
    
    helper_->revert(min_longitude_, min_latitude_);
    error = helper_->revert(max_longitude_, max_latitude_);
    double x = max_longitude_;
    double y = max_latitude_;

    helper_->convert(x, y);

    // cout << "[" << xpcmin_ << " " << ypcmin_ << "]-->[" << xpcmax_ << " " << ypcmax_ << "]" << endl;
    // cout << "[" << min_longitude_ << " " << min_latitude_ << "]-->[" << max_longitude_ << " " << max_latitude_ << "]" << endl;
    // cout << "[" << max_longitude_ << " " << max_latitude_ << "]-->[" << x << " " << y << "] --> " << error <<  endl;

    if (max_longitude_ < 0) {
        max_longitude_ += 360.;
    }
    

   


    
    magics::Polyline box;
    box.box(PaperPoint(xpcmin_, ypcmin_), PaperPoint(xpcmax_, ypcmax_));

    vector<magics::Polyline*> newbox;
    PCEnveloppe_->intersect(box, newbox);
    if (newbox.empty()) {
        MagLog::warning() << "Proj4 : the sub-area is not valid : use global view instead" << endl;
    }
    else {
        PCEnveloppe_ = newbox.front();
    }
    // reset
    setting_            = "corners";
    coordinates_system_ = "latlon";
}

void Proj4Projection::geos() {
    userEnveloppe_->clear();
    PCEnveloppe_->clear();
    // here we have to prepare the enveloppe!
    xpcmin_ = DBL_MAX;
    ypcmin_ = DBL_MAX;
    xpcmax_ = -DBL_MAX;
    ypcmax_ = -DBL_MAX;

    map<double, vector<double> > helper;

    for (int lat = projection_->minlat_; lat <= projection_->maxlat_; lat++) {
        helper.insert(make_pair(lat, vector<double>()));

        for (int lon = projection_->minlon_; lon <= projection_->maxlon_; lon++) {
            double x  = lon;
            double y  = lat;
            int error = helper_->convert(x, y);

            if (!error) {
                helper[lat].push_back(lon);
            }
        }
    }

    // now we create the envelope...
    for (map<double, vector<double> >::iterator lat = helper.begin(); lat != helper.end(); ++lat) {
        if (lat->second.empty())
            continue;

        add(lat->second.front(), lat->first);
    }
    // now reverse!

    for (map<double, vector<double> >::reverse_iterator lat = helper.rbegin(); lat != helper.rend(); ++lat) {
        if (lat->second.empty())
            continue;

        add(lat->second.back(), lat->first);
    }

    map<double, vector<double> >::iterator last = helper.begin();
    for (last = helper.begin(); last != helper.end(); ++last) {
        if (!last->second.empty())
            break;
    }

    for (vector<double>::reverse_iterator lon = last->second.rbegin(); lon != last->second.rend(); ++lon) {
        add(*lon, last->first);
    }
    
   
    gridMinLat_ = -90;
    gridMinLon_ = vertical_longitude_ -180;
    gridMaxLat_ = 90;
    gridMaxLon_ = vertical_longitude_ + 180;


    // Hre we have to make sure that the global area is defined between -180 and 180.

    if ( gridMaxLon_ > 180 ) {
        gridMaxLon_ = 180;
        gridMinLon_ = std::min(gridMaxLon_ - 360, gridMinLon_);
    }
    if ( gridMinLon_ < -180 ) {
        gridMinLon_ = -180;
        gridMaxLon_ = std::max(gridMinLon_ + 360, gridMaxLon_);
    }
    
}

magics::Polyline& Proj4Projection::getSimplePCBoundingBox() const {
    static magics::Polyline box;
    box.box(PaperPoint(xpcmin_, ypcmin_), PaperPoint(xpcmax_, ypcmax_));

    return box;
}

void Proj4Projection::tpers() {
    userEnveloppe_->clear();
    PCEnveloppe_->clear();
    // here we have to prepare the enveloppe!
    xpcmin_ = DBL_MAX;
    ypcmin_ = DBL_MAX;
    xpcmax_ = -DBL_MAX;
    ypcmax_ = -DBL_MAX;

    map<double, vector<double> > helper;

    // projection_view_latitude_
    // projection_view_longitude_

    double lastlon;
    int missing = -99999;
    double step = 0.1;
    for (double lat = projection_view_latitude_; lat <= 90; lat += step) {
        lastlon = missing;
        for (double lon = projection_view_longitude_; lon <= projection_view_longitude_ + 360; lon += step) {
            double x  = lon;
            double y  = lat;
            int error = helper_->convert(x, y);
            if (error) {
                // we reach a border!

                if (lastlon != missing) {
                    add(lastlon, lat);
                }
                break;
            }

            lastlon = lon;
        }
    }
    for (double lat = 90; lat >= projection_view_latitude_; lat -= step) {
        lastlon = missing;
        for (double lon = projection_view_longitude_; lon >= projection_view_longitude_ - 360; lon -= step) {
            double x  = lon;
            double y  = lat;
            int error = helper_->convert(x, y);
            if (error) {
                // we reach a border!
                if (lastlon != missing) {
                    add(lastlon, lat);
                }
                break;
            }

            lastlon = lon;
        }
    }

    for (double lat = projection_view_latitude_; lat >= -90; lat -= step) {
        lastlon = missing;
        for (double lon = projection_view_longitude_; lon >= projection_view_longitude_ - 360; lon -= step) {
            double x  = lon;
            double y  = lat;
            int error = helper_->convert(x, y);
            if (error) {
                // we reach a border!
                if (lastlon != missing) {
                    add(lastlon, lat);
                }
                break;
            }

            lastlon = lon;
        }
    }
    for (double lat = -90; lat <= projection_view_latitude_; lat += step) {
        lastlon = missing;
        for (double lon = projection_view_longitude_; lon <= projection_view_longitude_ + 360; lon += step) {
            double x  = lon;
            double y  = lat;
            int error = helper_->convert(x, y);
            if (error) {
                // we reach a border!
                if (lastlon != missing) {
                    add(lastlon, lat);
                }
                break;
            }

            lastlon = lon;
        }
    }

    userEnveloppe_->push_back(userEnveloppe_->front());
    PCEnveloppe_->push_back(PCEnveloppe_->front());

    gridMinLat_ = -90;
    gridMinLon_ = -200;
    gridMaxLat_ = 90;
    gridMaxLon_ = 200;
}

void Proj4Projection::boundingBox(double& xmin, double& ymin, double& xmax, double& ymax) const {
    if (!helper_) {
        projection_ = Epsg::find(definition_);
        helper_     = new LatLonProjP(projection_->definition());
    }
    


    ymin = gridMinLat_;
    xmin = gridMinLon_ - 5;  
    ymax = gridMaxLat_;  
    xmax = gridMaxLon_ + 5;
   
}

double Proj4Projection::getMinX() const {
    return gridMinLon_;
}

double Proj4Projection::getMinY() const {
    return gridMinLat_;
}

double Proj4Projection::getMaxX() const {
    return gridMaxLon_;
}

double Proj4Projection::getMaxY() const {
    return gridMaxLat_;
}

void Proj4Projection::setMinX(double x) {
    min_longitude_ = x;
}

void Proj4Projection::setMinY(double y) {
    min_latitude_ = y;
}

void Proj4Projection::setMaxX(double x) {
    max_longitude_ = x;
}

void Proj4Projection::setMaxY(double y) {
    max_latitude_ = y;
}

double Proj4Projection::getMinPCX() const {
    return xpcmin_;
}

double Proj4Projection::getMinPCY() const {
    return ypcmin_;
}

double Proj4Projection::getMaxPCX() const {
    return xpcmax_;
}

double Proj4Projection::getMaxPCY() const {
    return ypcmax_;
}

double Proj4Projection::getExtendedMinPCX() const {
    return xpcmin_ - xgutter_;
}

double Proj4Projection::getExtendedMinPCY() const {
    return ypcmin_- ygutter_;
}

double Proj4Projection::getExtendedMaxPCX() const {
    return xpcmax_ + xgutter_;
}

double Proj4Projection::getExtendedMaxPCY() const {
    return ypcmax_ + ygutter_;
}

void Proj4Projection::fill(double& width, double& height) {
    Transformation::fill(width, height);
    setting_       = "projection";
    min_longitude_ = xpcmin_;
    min_latitude_  = ypcmin_;
    max_longitude_ = xpcmax_;
    max_latitude_  = ypcmax_;
}
void Proj4Projection::gridLongitudes(const GridPlotting& grid) const {
    vector<double> longitudes = grid.longitudes();

    const double step = 0.5;
    // longitudes.push_back(180);
    const vector<double>::const_iterator lon_end = longitudes.end();
    // Always use full globe !
    double min = -90;
    double max = 90;
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != lon_end; ++lon) {
        magics::Polyline poly;
        poly.setAntiAliasing(false);

        for (double lat = min; lat < max; lat += step) {
            PaperPoint p = (*this)(UserPoint(*lon, lat));
            if (PCEnveloppe_->within(p)) {
                poly.push_back(p);
            }
            else {
                grid.add(poly);

                poly = magics::Polyline();
                poly.setAntiAliasing(false);
            }
        }

        grid.add(poly);
    }

    grid.addFrame(*PCEnveloppe_);
}

void Proj4Projection::gridLatitudes(const GridPlotting& grid) const {
    const vector<double>& latitudes = grid.latitudes();

    const double step                            = 0.5;
    const vector<double>::const_iterator lat_end = latitudes.end();

    // Always use full globe !
    double min = -180;
    double max = 360;

    for (vector<double>::const_iterator lat = latitudes.begin(); lat != lat_end; ++lat) {
        magics::Polyline poly;
        poly.setAntiAliasing(false);
        for (double lon = min; lon <= max; lon += step) {
            PaperPoint p = (*this)(UserPoint(lon, *lat));
            if (PCEnveloppe_->within(p))
                poly.push_back(p);
            else {
                grid.add(poly);
                poly = magics::Polyline();
                poly.setAntiAliasing(false);
            }
        }

        grid.add(poly);
    }

    grid.addFrame(*PCEnveloppe_);
}

void Proj4Projection::labels(const LabelPlotting& label, DrawingVisitor& visitor) const {
    if (projection_->method_ == "simple")
        return;
    vector<double> pro4_longitudes;
    pro4_longitudes.push_back(0);
    pro4_longitudes.push_back(90);
    pro4_longitudes.push_back(-90);
    // pro4_longitudes.push_back(180);
    // pro4_longitudes.push_back(-180);
    const vector<double>& longitudes = label.longitudes();
    const vector<double>& latitudes  = label.latitudes();
    for (vector<double>::const_iterator lat = latitudes.begin(); lat != latitudes.end(); ++lat) {
        for (vector<double>::iterator lon = pro4_longitudes.begin(); lon != pro4_longitudes.end(); ++lon) {
            vector<double>::const_iterator ilon = find(longitudes.begin(), longitudes.end(), *lon);
            if (ilon == longitudes.end())
                continue;

            UserPoint point(*lon, *lat);
            PaperPoint xy = (*this)(point);

            if (!in(xy))
                continue;

            Text* text = new Text();
            label.add(text);
            text->setText(writeLatitude(point));
            text->push_back(xy);
            text->setBlanking(true);
        }
    }
}

void Proj4Projection::labels(const LabelPlotting& label, LeftAxisVisitor& visitor) const {
    if (projection_->method_ == "simple") {
        const vector<double>& latitudes = label.latitudes();

        for (unsigned int lat = 0; lat < latitudes.size(); lat++) {
            if (latitudes[lat] < gridMinLat_)
                continue;
            if (latitudes[lat] > gridMaxLat_)
                continue;
            double lon = max_longitude_ - ((max_longitude_ - min_longitude_) * .1);
            UserPoint point(lon, latitudes[lat]);
            PaperPoint xy = (*this)(point);
            if (!in(xy))
                continue;
            Text* text = new Text();
            label.add(text);
            text->setText(writeLatitude(point));
            text->push_back(xy);
            text->setJustification(Justification::RIGHT);
            text->setVerticalAlign(VerticalAlign::HALF);
            text->setBlanking(true);
        }
    }
    else {
        double x = xpcmax_ - ((xpcmax_ - xpcmin_) * .1);
        // we calculate the intersection of the longitudes with the left side
        verticalLabels(label, xpcmin_, x, Justification::RIGHT);
    }
}

void Proj4Projection::labels(const LabelPlotting& label, RightAxisVisitor& visitor) const {
    if (projection_->method_ == "simple") {
        const vector<double>& latitudes = label.latitudes();
        for (unsigned int lat = 0; lat < latitudes.size(); lat++) {
            if (latitudes[lat] < gridMinLat_)
                continue;
            if (latitudes[lat] > gridMaxLat_)
                continue;
            double lon = min_longitude_ + ((max_longitude_ - min_longitude_) * .1);
            UserPoint point(lon, latitudes[lat]);
            PaperPoint xy = (*this)(point);
            if (!in(xy))
                continue;
            Text* text = new Text();
            label.add(text);
            text->setText(writeLatitude(point));
            text->push_back(xy);
            text->setJustification(Justification::LEFT);
            text->setVerticalAlign(VerticalAlign::HALF);
            text->setBlanking(true);
        }
    }
    else {
        // we calculate the intersection of the longitudes with the right side
        double x = xpcmin_ + ((xpcmax_ - xpcmin_) * .1);
        verticalLabels(label, xpcmax_, x, Justification::LEFT);
    }
}

void Proj4Projection::labels(const LabelPlotting& label, BottomAxisVisitor& visitor) const {
    if (projection_->method_ == "simple") {
        const vector<double>& longitudes = label.longitudes();
        const double lat                 = min_latitude_ + (max_latitude_ - min_latitude_) * .8;
        for (unsigned int lon = 0; lon < longitudes.size(); lon++) {
            if (longitudes[lon] < gridMinLon_)
                continue;
            if (longitudes[lon] > gridMaxLon_)
                continue;
            UserPoint point(longitudes[lon], lat);
            PaperPoint xy = (*this)(point);
            if (!in(xy))
                continue;
            Text* text = new Text();
            label.add(text);
            text->setText(writeLongitude(point));

            text->push_back(xy);
            text->setJustification(Justification::CENTRE);
            text->setVerticalAlign(VerticalAlign::TOP);
            text->setBlanking(true);
        }
    }
    else {
        // we calculate the intersection of the longitudes with the right side
        double y = ypcmin_ + ((ypcmax_ - ypcmin_) * .8);
        horizontalLabels(label, ypcmin_, y, VerticalAlign::TOP);
    }
}

inline double CA(PaperPoint& p1, PaperPoint& p2) {
    return (p2.x() - p1.x()) ? (p2.y() - p1.y()) / (p2.x() - p1.x()) : 0;
}

inline double CB(double a, PaperPoint& p) {
    return p.y() - a * p.x();
}

inline double CX(double a, double b, double y) {
    return (a) ? (y - b) / a : 0;
}

inline double CY(double a, double b, double x) {
    return (a * x) + b;
}

inline bool between(double x, double x1, double x2) {
    return (std::min(x1, x2) <= x && x <= std::max(x1, x2));
}

void Proj4Projection::verticalLabels(const LabelPlotting& label, double x, double pos, Justification justif) const {
    const vector<double>& longitudes = label.longitudes();
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon) {
        // find the equation of the line using 2 points : lon/-20 -->lon/ +20
        for (double lat1 = -90, lat2 = -80; lat2 < 90; lat1 += 10, lat2 += 10) {
            UserPoint geo1(*lon, lat1);
            UserPoint geo2(*lon, lat2);
            PaperPoint xy1 = (*this)(geo1);
            PaperPoint xy2 = (*this)(geo2);
            if (between(x, xy1.x_, xy2.x_)) {
                double a = CA(xy1, xy2);
                double b = CB(a, xy1);
                PaperPoint xy(x, CY(a, b, x));
                if (!in(xy))
                    continue;
                UserPoint geo;
                revert(xy, geo);
                xy.x(pos);
                if (!same(geo.x(), *lon))
                    continue;
                Text* text = new Text();
                label.add(text);
                text->setJustification(justif);
                text->setVerticalAlign(VerticalAlign::HALF);
                text->setText(writeLongitude(geo));
                text->push_back(xy);
            }
        }
    }
}

void Proj4Projection::horizontalLabels(const LabelPlotting& label, double y, double pos, VerticalAlign align) const {
    const vector<double>& longitudes = label.longitudes();
    for (vector<double>::const_iterator lon = longitudes.begin(); lon != longitudes.end(); ++lon) {
        // find the equation of the line using 2 points : lon/-20 -->lon/ +20
        for (double lat1 = -90, lat2 = -80; lat2 < 90; lat1 += 10, lat2 += 10) {
            UserPoint geo1(*lon, lat1);
            UserPoint geo2(*lon, lat2);
            PaperPoint xy1 = (*this)(geo1);
            PaperPoint xy2 = (*this)(geo2);
            if (between(y, xy1.y_, xy2.y_)) {
                double a = CA(xy1, xy2);
                double b = CB(a, xy1);
                PaperPoint xy(CX(a, b, y), y);
                if (!in(xy))
                    continue;
                UserPoint geo;
                revert(xy, geo);
                xy.y(pos);
                if (!same(geo.x(), *lon))
                    continue;
                Text* text = new Text();
                label.add(text);
                text->setJustification(Justification::CENTRE);
                text->setVerticalAlign(align);
                text->setText(writeLongitude(geo));
                text->push_back(xy);
            }
        }
    }
}

void Proj4Projection::labels(const LabelPlotting& label, TopAxisVisitor& visitor) const {
    if (projection_->method_ == "simple") {
        const vector<double>& longitudes = label.longitudes();
        const double lat                 = min_latitude_ + (max_latitude_ - min_latitude_) * .2;
        for (unsigned int lon = 0; lon < longitudes.size(); lon++) {
            if (longitudes[lon] < gridMinLon_)
                continue;
            if (longitudes[lon] > gridMaxLon_)
                continue;
            UserPoint point(longitudes[lon], lat);
            PaperPoint xy = (*this)(point);
            if (!in(xy))
                continue;
            Text* text = new Text();
            label.add(text);
            text->setText(writeLongitude(point));
            text->push_back(xy);
            text->setJustification(Justification::CENTRE);
            text->setVerticalAlign(VerticalAlign::BOTTOM);
            text->setBlanking(true);
        }
    }
    else {
        // we calculate the intersection of the longitudes with the right side
        double y = ypcmin_ + ((ypcmax_ - ypcmin_) * .2);
        horizontalLabels(label, ypcmax_, y, VerticalAlign::BOTTOM);
    }
}

void Proj4Projection::reprojectComponents(double& x, double& y, pair<double, double>& components) const {
    const double speed = sqrt((components.first * components.first) + (components.second * components.second));
    const double angle = atan2(components.second, components.first);
    double ppx         = x + cos(angle);
    double ppy         = y + sin(angle);

    fast_reproject(ppx, ppy);
    fast_reproject(x, y);

    const double rotation = atan2((ppy - y), (ppx - x));

    // we the angle and the spped we compute u/v...
    components.first  = speed * cos(rotation);
    components.second = speed * sin(rotation);
}

void myprint(double x, double y, bool next = false) {
    MagLog::dev() << "[" << x << ", " << y << "]";
    if (next)
        MagLog::dev() << "--->";
    else
        MagLog::dev() << endl;
}

void Proj4Projection::reprojectSpeedDirection(const PaperPoint& point, pair<double, double>& wind) const {
    double x = point.x_;
    double y = point.y_;

    double u = x + (sin(RAD(wind.second)));
    double v = y + (cos(RAD(wind.second)));

    fast_reproject(x, y);
    fast_reproject(u, v);

    double rotation = atan2((u - x), (v - y));

    wind.second = DEG(rotation);
}

void Proj4Projection::revert(const vector<std::pair<double, double> >& in,
                             vector<std::pair<double, double> >& out) const {
    const_cast<Proj4Projection*>(this)->init();
    out.reserve(in.size());
    for (vector<std::pair<double, double> >::const_iterator pt = in.begin(); pt != in.end(); ++pt) {
        double x = pt->first;
        double y = pt->second;
        PaperPoint p(x, y);

        // Trying to remove this test, to make the wind tiles working
        // if (PCEnveloppe_->within(p) == false) {
        //     out.push_back(make_pair(HUGE_VAL, HUGE_VAL));
        //     continue;
        // }

        int error = helper_->revert(x, y);

        if (error) {
            //The point is outside the projection, we return a HUGE_VAL point
            out.push_back(make_pair(HUGE_VAL, HUGE_VAL));
        }
        else {
            double lon = x;
            if (lon > gridMaxLon_)
                lon -= 360.;
            else if (lon < gridMinLon_)
                lon += 360.;
            double lat = y;
            out.push_back(make_pair(lon, lat));
        }
    }
}

void Proj4Projection::coastSetting(map<string, string>& setting, double abswidth, double absheight) const {
    // work out the ratios of geographical to paper lengths
    // const double xratio = ( xpcmax_ - xpcmin_ ) / abswidth;
    // const double yratio = ( ypcmax_ - ypcmin_ ) / absheight;

    // choose the smallest (smaller ratio means more detail required)
    const double area = (xpcmax_ - xpcmin_) * (ypcmax_ - ypcmin_);

    double ratio = area / (abswidth * absheight);

    std::string resol = "110m";
    if (ratio < 1000000)  // highest resolution
    {
        resol = "10m";
    }
    else if (ratio < 3000000)  // medium resolution
    {
        resol = "50m";
    }

    setting["resolution"] = resol;
    setting["land"]       = resol + "/ne_" + resol + "_land";
    setting["ocean"]      = resol + "/ne_" + resol + "_ocean";
    setting["coast"]      = resol + "/ne_" + resol + "_coastline";
    setting["rivers"]     = resol + "/ne_" + resol + "_rivers_lake_centerlines";
    setting["boundaries"] = resol + "/ne_" + resol + "_admin_0_boundary_lines_land";
    //    setting["administrative_boundaries"] = resol + "/ne_" + resol +
    //    "_admin_1_states_provinces";

    //! \note Administraive borders hardcoded to 10m resolution (low res version
    //! do not contain all info)
    setting["administrative_boundaries"] = "10m/ne_10m_admin_1_states_provinces_lines";
}

void Proj4Projection::visit(MetaDataVisitor& visitor, double left, double top, double width, double height,
                            double iwidth, double iheight) {
    ostringstream java;
    double w = xpcmax_ - xpcmin_;
    double h = ypcmax_ - ypcmin_;
    java << "{";
    java << "\"name\" : \"" << definition_ << "\",";
    java << "\"definition\" : \"" << definition_ << "\",";
    java << "\"proj4_definition\" : \"" << projection_->definition_ << "\",";
    java << "\"top\" : \"" << top << "\",";
    java << "\"left\" : \"" << left << "\",";
    java << "\"width\" : \"" << width << "\",";
    java << "\"height\" : \"" << height << "\",";
    java << "\"img_width\" : \"" << iwidth << "\",";
    java << "\"img_height\" : \"" << iheight << "\",";
    java << "\"pcxmin\" : \"" << xpcmin_ << "\",";
    java << "\"pcymin\" : \"" << ypcmin_ << "\",";
    java << "\"pcwidth\" : \"" << w << "\",";
    java << "\"pcheight\" : \"" << h << "\"";
    java << "}";
    visitor.add("projection", java.str());
    ostringstream wf;
    wf << (w / width) << endl;
    wf << "0\n0\n";
    wf << -(h / height) << endl;
    wf << getMaxPCY() - (h / height) / 2 << endl;
    wf << getMinPCX() + (w / width) / 2 << endl;
    visitor.add("world_file", wf.str());
}

MatrixHandler* Proj4Projection::prepareData(const AbstractMatrix& matrix) const {
    return new GeoBoxMatrixHandler(matrix, *this);
}

bool Proj4Projection::fast_reproject(double& x, double& y) const {
    int factor = 0;
    if (wraparound_) {
        factor = int(x / width_);
        x -= (factor - width_);
    }

    int error = helper_->convert(x, y);

    if (error) {
        x = HUGE_VAL;
        y = HUGE_VAL;
        // MagLog::warning()  << pj_strerrno(error) << " for " << x << " " << y <<
        // endl;
        return false;
    }
    if (wraparound_)
        x += (factor - pwidth_);
    return true;
}

double Proj4Projection::patchDistance(double res) const {
    double x1 = 0;
    double y1 = 60;
    double x2 = 0;
    double y2 = 61;
    fast_reproject(x1, y1);
    fast_reproject(x2, y2);

    double degree = ((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2));
    return 1000000000;
}

void Proj4Projection::collect(MetaDataCollector& collector) const {
    collector["Projection"]        = definition_;
    collector["Proj4  Definition"] = projection_->definition();
}

void Proj4Projection::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const {
    map<string, string> def;
    def["subpage_map_projection"]        = definition_;
    def["subpage_map_area_definition"]   = "corners";
    def["subpage_lower_left_longitude"]  = tostring(ll.x_);
    def["subpage_lower_left_latitude"]   = tostring(ll.y_);
    def["subpage_upper_right_longitude"] = tostring(ur.x_);
    def["subpage_upper_right_latitude"]  = tostring(ur.y_);
    ::toxml(out, def);
    out = "{" + out + "}";
}

void Proj4Projection::setDefinition(const string& json) {
    if (json.empty())
        return;

    MagJSon helper;
    helper.interpret(json);

    XmlNode node = **helper.tree_.begin();
    node.name("");
    set(node);
}

void Proj4Automatic::setMinMaxX(double min, double max) {
    min_longitude_ = min;
    max_longitude_ = max;
}

void Proj4Automatic::setMinMaxY(double min, double max) {
    min_latitude_ = min;
    max_latitude_ = max;
    if (min_latitude_ > 45) {
        definition_         = "polar_north";
        vertical_longitude_ = (min_longitude_ + max_longitude_) / 2;
        MagLog::dev() << "Set Vertical longitude " << vertical_longitude_ << endl;
        // map_hemisphere_     = "north";
        setting_ = "corners";
        min_latitude_ -= 5;
    }
    else if (max_latitude_ < -45) {
        definition_         = "polar_south";
        vertical_longitude_ = (min_longitude_ + max_longitude_) / 2;
        MagLog::dev() << "Set Vertical longitude " << vertical_longitude_ << endl;
        // map_hemisphere_     = "north";
        setting_ = "corners";
        max_latitude_ += 10;
        double swap    = min_longitude_;
        min_longitude_ = max_longitude_;
        max_longitude_ = swap;
    }
    else {
        definition_ = "EPSG:4326";
        setting_    = "corners";
    }
    init_ = true;
    fill(width_, height_);

    // Now apply teh aspect ratio and reintialise!

    min_latitude_  = gridMinLat_;
    min_longitude_ = gridMinLon_;
    max_latitude_  = gridMaxLat_;
    max_longitude_ = gridMaxLon_;

    init();
}

void Proj4Automatic::init() {
    if (init_)
        Proj4Projection::init();
}

Proj4Automatic::Proj4Automatic() : Proj4Projection("automatic"), init_(false) {}

void Proj4Automatic::aspectRatio(double& width, double& height) {
    width_  = width;
    height_ = height;
}

void Proj4Automatic::setNewPCBox(double minx, double miny, double maxx, double maxy) {
    gridMinLon_ = minx;
    gridMinLat_ = miny;
    int error   = helper_->revert(gridMinLon_, gridMinLat_);
    if (error) {
        MagLog::error() << "Error Proj4Automatic::setNewPCBox to check " << endl;
    }

    gridMaxLon_ = maxx;
    gridMaxLat_ = maxy;
    error       = helper_->revert(gridMaxLon_, gridMaxLat_);
    if (error) {
        MagLog::error() << "Error Proj4Automatic::setNewPCBox to check " << endl;
    }
}
