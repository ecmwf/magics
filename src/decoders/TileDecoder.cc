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
#ifdef HAVE_NETCDF
#include "NetcdfData.h"
#endif
#include "Timer.h"

using namespace magics;

TileDecoder::~TileDecoder() {}


TileDecoder::TileDecoder() {}


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
        parent = buildSharePath("tiles");
    }
    out << parent << "/weight-" << grid_ << "-" << projection() << "-z" + tostring(z_) << ".nc";
    return out.str();
}

string TileDecoder::positions() {
    ostringstream out;
    string parent = getEnvVariable("MAGPLUS_TILE");
    if (parent.empty()) {
        parent = buildSharePath("tiles");
    }
    out << parent << "/wind-" << grid_ << "-" << projection() << "-z" + tostring(z_) << ".nc";

    return out.str();
}

string TileDecoder::positions_symbols() {
    ostringstream out;
    string parent = getEnvVariable("MAGPLUS_TILE");
    if (parent.empty()) {
        parent = buildSharePath("tiles");
    }
    out << parent << "/symbol-" << grid_ << "-" << projection() << "-z" + tostring(z_) << ".nc";
    file_ = ifstream(out.str());
    if (file_.good()) {
        file_.close();
        return out.str();
    }
    file_.close();
    return positions();
}

bool TileDecoder::ok() {
    FILE* in = fopen(file_name_.c_str(), "rb");
    if (!in) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(file_name_);
        }
        MagLog::error() << "ERROR: unable to open file" << file_name_ << endl;
        return false;
    }
    /* create new handle from a message in a file*/
    int error;
    handle_ = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (handle_ == NULL) {
        MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
        return false;
    }
    char val[1024];
    size_t length = 1024;
    error         = grib_get_string(handle_, "gridName", val, &length);
    grid_         = string(val);

    string path = weights();
    file_       = ifstream(path);
    if (!file_.good()) {
        file_.close();
        return false;
    }

    file_.close();
    int count;
    error = codes_count_in_file(0, in, &count);


    if (loop_) {
        int error;
        codes_handle* handle = handle_;

        int count;
        error = codes_count_in_file(0, in, &count);


        for (int i = 0; i < count; i++) {
            entries_.push_back(handle);
            handle = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
        }
        entry_ = entries_.begin();
    }
    return true;
}

double compute(double* values, double* weights, int nb, double total) {
    double val = 0;


    for (int i = 0; i < nb; i++) {
        val += values[i] * weights[i];
    }

    val /= total;
    return val;
}

void TileDecoder::customisedPoints(const Transformation& transformation, const std::set<string>& n,
                                   CustomisedPointsList& out, bool all) {
    string path = positions();
    Timer timer("Tile", path);

#ifdef HAVE_NETCDF
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

    FILE* in = fopen(file_name_.c_str(), "rb");
    if (!in) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(file_name_);
        }
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

    for (auto b = values.begin(); b != values.end(); ++b) {
        double lat = *b;
        ++b;
        double lon = *b;
        ++b;
        double i = *b;

        if (i != 0) {
            if (lon > 180)
                lon -= 360;
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
            point->tile(true);
            ux++;
            vx++;
            lat++;
            lon++;
        }
    }
#else
    MagLog::warning() << " TileDecoder> CANNOT work because NetCDF was DISABLED in Magics!" << endl;
#endif
}

PointsHandler& TileDecoder::points(const Transformation& t, bool) {
    string path = positions_symbols();
    Timer timer("Tile", path);

#ifdef HAVE_NETCDF
    Netcdf netcdf(path, "index");

    map<string, string> first, last;
    first["x"] = tostring(x_);
    first["y"] = tostring(y_);
    last["x"]  = tostring(x_);
    last["y"]  = tostring(y_);
    // vector<double> bbox;
    static vector<double> latitudes;
    static vector<double> longitudes;
    static vector<double> values;
    vector<int> index;

    int error;

    FILE* in = fopen(file_name_.c_str(), "rb");
    if (!in) {
        if (MagicsGlobal::strict()) {
            throw CannotOpenFile(file_name_);
        }
        MagLog::error() << "ERROR: unable to open file" << file_name_ << endl;
        pointsHandlers_.push_back(new PointsHandler(points_));
        return *(pointsHandlers_.back());
    }


    /* create new handle from a message in a file*/
    codes_handle* f = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (f == NULL) {
        MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
        pointsHandlers_.push_back(new PointsHandler(points_));
        return *(pointsHandlers_.back());
    }


    if (latitudes.size() == 0) {
        int nbpoints = netcdf.getDimension("points");
        // netcdf.get("bounding-box", bbox, first, last);
        netcdf.get("index", values, first, last);


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
    }

    double scaling = 1;
    double offset  = 0;
    scaling_offset(f, scaling, offset);

    {
        vector<double> values;
        values.reserve(index.size());

        codes_get_double_elements(f, "values", &index.front(), index.size(), &values.front());


        auto lat = latitudes.begin();
        auto lon = longitudes.begin();
        auto val = values.begin();

        while (lat != latitudes.end()) {
            points_.push_back(new UserPoint(*lon, *lat, (*val * scaling) + offset));
            val++;
            lat++;
            lon++;
        }
    }
    pointsHandlers_.push_back(new PointsHandler(points_));
    return *(pointsHandlers_.back());
#else
    MagLog::warning() << " TileDecoder> CANNOT work because NetCDF was DISABLED in Magics!" << endl;
    NOTIMP;
#endif
}

/*!
 Class information are given to the output-stream.
*/
void TileDecoder::print(ostream& out) const {
    out << "TileDecoder[";
    out << "]";
}


void TileDecoder::scaling_offset(codes_handle* f, double& scaling, double& offset) {
    char tmp1[20], tmp2[256];
    size_t length1 = 20;
    size_t length2 = 26;

    map<string, double> offsets   = {{"K", -273.15}};
    map<string, double> scalings1 = {{"cbh", 1.0},
                                     {"hcct", 1.0},
                                     {"ceil", 1.0},
                                     {"pt", 1.0},
                                     {"kx", 1.0},
                                     {"totalx", 1.0},
                                     {"sund", 0.0002777777777777778},
                                     {"deg0l", 1.0},
                                     {"vis", 1.0},
                                     {"ceil", 1.0},
                                     {"capes", 1.0},
                                     {"mxcapes6", 1.0}};
    map<string, double> scalings  = {{"Pa", 0.01},
                                    {"gpm", 10.},
                                    {"kg kg**-1", 1000.0},
                                    {"m**2 s**-2", 0.0101971621297793},
                                    {"m", 1000.0},
                                    {"m of water equivalent", 1000},
                                    {"s**-1", 100000.0},
                                    {"m of water", 1000},
                                    {"K m**2 kg**-1 s**-1", 1000000.0}};

    int err = grib_get_string(f, "shortName", tmp1, &length1);
    string name(tmp1);

    if (!scaling_) {
        scaling = scaling_factor_;
        offset  = scaling_offset_;
        return;
    }
    auto scale = scalings1.find(name);
    if (scale != scalings1.end()) {
        scaling = scale->second;
    }
    else {
        err = grib_get_string(f, "units", tmp2, &length2);
        if (!err) {
            string units(tmp2);
            auto off = offsets.find(units);
            if (off != offsets.end()) {
                offset = off->second;
            }
            auto sc = scalings.find(units);
            if (sc != scalings.end()) {
                scaling = sc->second;
            }
        }
    }
}

Data* TileDecoder::current() {
    if (!loop_)
        return this;
    if (entry_ != entries_.end()) {
        matrix_.clear();
        handle_ = *entry_;
        return this;
    }
    else
        return 0;
}

Data* TileDecoder::next() {
    if (!loop_)
        return 0;
    ++entry_;
    if (entry_ != entries_.end()) {
        matrix_ = Matrix();
        handle_ = *entry_;
        return this;
    }
    else
        return 0;
}


void TileDecoder::decode() {
    if (matrix_.size())
        return;

    string path = weights();

    Timer timer("Tile", path);
#ifdef HAVE_NETCDF
    Netcdf netcdf(path, "index");

    map<string, string> first, last;
    first["x"] = tostring(x_);
    first["y"] = tostring(y_);
    last["x"]  = tostring(x_);
    last["y"]  = tostring(y_);

    static vector<double> bbox;
    static vector<double> latitudes;
    static vector<double> longitudes;

    static vector<double> dindex;
    static vector<double> distances;

    int nblat = netcdf.getDimension("lat") - 1;
    int nblon = netcdf.getDimension("lon") - 1;

    if (bbox.empty()) {
        netcdf.get("bounding-box", bbox, first, last);

        int error;
        FILE* in = fopen(file_name_.c_str(), "rb");
        if (!in) {
            if (MagicsGlobal::strict()) {
                throw CannotOpenFile(file_name_);
            }
            MagLog::error() << "ERROR: unable to create handle from file" << file_name_ << endl;
            return;
        }

        // Adding a gutter of 5 points to avoid borders in tiles
        int miny = std::min(bbox[1], bbox[3]);
        miny     = std::max(0, miny - 5);
        int maxy = std::max(bbox[1], bbox[3]);
        maxy     = std::min(nblat, maxy + 5);
        int minx = std::min(bbox[0], bbox[2]);
        minx     = std::max(0, minx - 5);
        int maxx = std::max(bbox[0], bbox[2]);
        maxx     = std::min(nblon, maxx + 5);

        first["lat"] = tostring(miny);
        first["lon"] = tostring(minx);
        last["lat"]  = tostring(maxy);
        last["lon"]  = tostring(maxx);

        netcdf.get("lat", latitudes, first, last);
        netcdf.get("lon", longitudes, first, last);
        netcdf.get("index", dindex, first, last);
        netcdf.get("distances", distances, first, last);
    }

    int index[4];
    double weight[4];
    double values[4];

    double missing = -std::numeric_limits<double>::max();
    int err        = codes_get_double(handle_, "missingValue", &missing);
    if (err) {
        missing = -std::numeric_limits<double>::max();
    }


    double offset  = 0;
    double scaling = 1;

    scaling_offset(handle_, scaling, offset);
    // check the units for scaling!

    matrix_.missing(missing);

    auto d = distances.begin();
    matrix_.reserve(dindex.size() / 4);
    int c = 0;

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

    codes_get_double_elements(handle_, "values", &iindex.front(), iindex.size(), &dvalues.front());

    auto val = dvalues.begin();
    for (auto l = dindex.begin(); l != dindex.end(); ++l) {
        if (*l < 0)
            cvalues.push_back(missing);
        else {
            if ((*val) == missing)
                cvalues.push_back(missing);
            else
                cvalues.push_back((*val * scaling) + offset);
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
#else
    MagLog::warning() << " TileDecoder> CANNOT work because NetCDF was DISABLED in Magics!" << endl;
#endif
}
