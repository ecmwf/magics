/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TileDecoder.h
    \brief Implementation of the Template class TileDecoder.

    Magics Team - ECMWF 2004

    Started: Thu 25-Mar-2004

    Changes:

*/


#include "TileDecoder.h"
#include <limits>
#include "Factory.h"
#include "NetcdfData.h"
#include "Timer.h"

using namespace magics;

TileDecoder::~TileDecoder() {}


#include "eccodes.h"


TileDecoder::TileDecoder() {
    cout << "New Tile Decoder" << endl;
}


string TileDecoder::projection() {
    map<string, string> projections = {
        {"cylindrical", "4326"}, {"EPSG:4326", "4326"}, {"EPSG:32661", "32661"}, {"EPSG:32761", "32761"}};
    auto proj = projections.find(projection_);
    if (proj != projections.end())
        return proj->second;
    return "none";
}


string TileDecoder::weights() {
    ostringstream out;
    string parent = getEnvVariable("MAGPLUS_TILE");
    if (parent.empty()) {
        parent = getEnvVariable("MAGPLUS_HOME") + "/share/magics/tiles";
    }
    out << parent << "/weight-" << grid_ << "-" << projection() << "-z" + tostring(z_) << ".nc";
    return out.str();
}

string TileDecoder::positions() {
    ostringstream out;
    string parent = getEnvVariable("MAGPLUS_TILE");
    if (parent.empty()) {
        parent = getEnvVariable("MAGPLUS_HOME") + "/share/magics/tiles";
    }
    out << parent << "/wind-" << grid_ << "-" << projection() << "-z" + tostring(z_) << ".nc";

    return out.str();
}

bool TileDecoder::ok() {
    FILE* in = fopen(file_name_.c_str(), "r");
    if (!in) {
        MagLog::error() << "ERROR: unable to open file" << file_name_ << endl;
        return false;
    }
    /* create new handle from a message in a file*/
    int error;
    codes_handle* h = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (h == NULL) {
        MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
        return false;
    }
    char val[1024];
    size_t length = 1024;
    error         = grib_get_string(h, "gridName", val, &length);
    grid_         = string(val);
    codes_handle_delete(h);
    string path = weights();
    file_       = ifstream(path);
    if (file_.good()) {
        file_.close();
        return true;
    }
    file_.close();
    return false;
}

double compute(double* values, double* weights, int nb, double total) {
    double val = 0;


    for (int i = 0; i < nb; i++) {
        // cout << " " << values[i] << endl;
        val += values[i] * weights[i];
    }

    val /= total;
    return val;
}

void TileDecoder::customisedPoints(const Transformation& transformation, const std::set<string>& n,
                                   CustomisedPointsList& out, bool all) {
    string path = positions();
    Timer timer("Tile", path);
    cout << "Tiles --> " << path << endl;
    Netcdf netcdf(path, "index");

    map<string, string> first, last;
    first["x"] = tostring(x_);
    first["y"] = tostring(y_);
    last["x"]  = tostring(x_);
    last["y"]  = tostring(y_);
    vector<double> bbox;
    vector<double> latitudes;
    vector<double> longitudes;
    vector<double> values;
    vector<int> index;


    int error;


    FILE* in = fopen(file_name_.c_str(), "r");
    if (!in) {
        MagLog::error() << "ERROR: unable to open file" << file_name_ << endl;
        return;
    }


    /* create new handle from a message in a file*/
    codes_handle* u = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (u == NULL) {
        MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
        return;
    }

    codes_handle* v = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (v == NULL) {
        MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
        return;
    }


    int nbpoints = netcdf.getDimension("points");
    // netcdf.get("bounding-box", bbox, first, last);
    netcdf.get("index", values, first, last);

    // for (auto b = bbox.begin(); b != bbox.end(); ++b)
    //  cout << "found BBOX" << *b << endl;
    cout << "FOUND DIM " << nbpoints << endl;
    for (auto b = values.begin(); b != values.end(); ++b) {
        double lat = *b;
        ++b;
        double lon = *b;
        ++b;
        double i = *b;


        if (i != 0) {
            if (lon > 180)
                lon -= 360;
            // transformation.fast_reproject(lon, lat);

            latitudes.push_back(lat);
            longitudes.push_back(lon);
            index.push_back(i);
        }
    }


    {
        vector<double> uc;
        uc.reserve(index.size());
        vector<double> vc;
        vc.reserve(index.size());

        codes_get_double_elements(u, "values", &index.front(), index.size(), &uc.front());
        codes_get_double_elements(v, "values", &index.front(), index.size(), &vc.front());


        auto lat = latitudes.begin();
        auto lon = longitudes.begin();
        auto ux  = uc.begin();
        auto vx  = vc.begin();

        while (lat != latitudes.end()) {
            CustomisedPoint* point = new CustomisedPoint(*lon, *lat, "");
            point->insert(make_pair("x_component", *ux));
            point->insert(make_pair("y_component", *vx));
            out.push_back(point);
            ux++;
            vx++;
            lat++;
            lon++;
        }
    }
}

PointsHandler& TileDecoder::points(const Transformation& t, bool) {}

/*!
 Class information are given to the output-stream.
*/
void TileDecoder::print(ostream& out) const {
    out << "TileDecoder[";
    out << "]";
}


void TileDecoder::decode() {
    if (matrix_.size())
        return;

    string path = weights();
    cout << "Tiles --> " << path << endl;
    Timer timer("Tile", path);

    Netcdf netcdf(path, "index");

    map<string, double> offsets = {{"K", -273.15}};

    map<string, string> first, last;
    first["x"] = tostring(x_);
    first["y"] = tostring(y_);
    last["x"]  = tostring(x_);
    last["y"]  = tostring(y_);
    vector<double> bbox;
    vector<double> latitudes;
    vector<double> longitudes;

    vector<double> dindex;
    vector<double> distances;

    int size = netcdf.getDimension("lat");

    cout << "SIZE-->" << size << endl;

    netcdf.get("bounding-box", bbox, first, last);
    for (auto b = bbox.begin(); b != bbox.end(); ++b)
        cout << "found BBOX" << *b << endl;

    int error;
    FILE* in = fopen(file_name_.c_str(), "r");
    if (!in) {
        MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
        return;
    }

    int miny = std::min(bbox[1], bbox[3]);
    if (miny > 0)
        miny = miny - 1;
    int maxy = std::max(bbox[1], bbox[3]);
    if (maxy < size - 1)
        maxy = maxy + 1;
    int minx = std::min(bbox[0], bbox[2]);
    if (minx > 0)
        minx = minx - 1;
    int maxx = std::max(bbox[0], bbox[2]);
    if (maxx < size - 1)
        maxx = maxx + 1;

    first["lat"] = tostring(miny);
    first["lon"] = tostring(minx);
    last["lat"]  = tostring(maxy);
    last["lon"]  = tostring(maxx);

    netcdf.get("lat", latitudes, first, last);
    netcdf.get("lon", longitudes, first, last);
    netcdf.get("index", dindex, first, last);
    netcdf.get("distances", distances, first, last);


    int index[4];
    double weight[4];
    double values[4];

    codes_handle* f = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);

    if (f == NULL) {
        MagLog::error() << "ERROR: nable to create handle from fil" << file_name_ << endl;
    }

    double missing = -std::numeric_limits<double>::max();

    char tmp[20];
    size_t length = 20;

    double offset = 0;
    int err       = grib_get_string(f, "units", tmp, &length);
    if (!err) {
        string units(tmp);
        auto off = offsets.find(units);
        if (off != offsets.end()) {
            offset = off->second;
            cout << "Use Offset-->" << offset << endl;
        }
    }


    // check the units for scaling!


    matrix_.missing(missing);

    auto d = distances.begin();
    matrix_.reserve(dindex.size() / 4);
    int c = 0;
    // cout << "SIZE-->" << dindex.size() / 4 << "---" << distances.size() << "---" << longitudes.size() *
    // latitudes.size()
    //    << endl;
    vector<int> iindex;

    vector<double> dvalues;
    vector<double> cvalues;

    iindex.reserve(dindex.size());
    dvalues.reserve(dindex.size());
    cvalues.reserve(dindex.size());
    for (auto l = dindex.begin(); l != dindex.end(); ++l) {
        if (*l > -1)
            iindex.push_back(*l);
    }


    codes_get_double_elements(f, "values", &iindex.front(), iindex.size(), &dvalues.front());


    auto val = dvalues.begin();
    for (auto l = dindex.begin(); l != dindex.end(); ++l) {
        if (*l < 0)
            cvalues.push_back(missing);
        else {
            cvalues.push_back(*val + offset);
            val++;
        }
    }


    // cleaning values


    int ind = 0;
    while (ind < distances.size()) {
        double total = 0;
        int nb       = 0;

        for (int i = 0; i < 4; i++) {
            if (cvalues[ind] != missing && !same(distances[ind], 0)) {
                weight[i] = 1 / distances[ind];
                values[i] = cvalues[ind];
                total += weight[i];
                nb++;
            }
            ind++;
        }


        double val = nb ? compute(values, weight, nb, total) : missing;
        c++;
        matrix_.push_back(val);
    }


    for (auto lon = longitudes.begin(); lon != longitudes.end(); ++lon) {
        matrix_.columnsAxis().push_back(*lon);
    }

    for (auto lat = latitudes.begin(); lat != latitudes.end(); ++lat) {
        matrix_.rowsAxis().push_back(*lat);
    }

    matrix_.setMapsAxis();
}
