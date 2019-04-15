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

using namespace magics;

TileDecoder::~TileDecoder() {}


#include "eccodes.h"


class PointTile {
public:
    PointTile(int index1, int index2, int index3, int index4, double weight1, double weight2, double weight3,
              double weight4, double total) :
        total_(total) {
        indexes_.reserve(4);
        weights_.reserve(4);
        indexes_.push_back(index1);
        indexes_.push_back(index2);
        indexes_.push_back(index3);
        indexes_.push_back(index4);
        weights_.push_back(weight1);
        weights_.push_back(weight2);
        weights_.push_back(weight3);
        weights_.push_back(weight4);
    }
    vector<int> indexes_;
    vector<double> weights_;
    double total_;

    double compute(vector<double>& values) {
        double val  = 0;
        auto weight = weights_.begin();
        for (int i = 0; i < 4; i++) {
            cout << " " << values[i] << endl;
            val += values[i] * (*weight);
            weight++;
        }

        val /= total_;
        return val;
    }
};
class Tile {
public:
    Tile() {}
    Tile(int z, int x, int y, int size, double xmin, double ymin, double xmax, double ymax) :
        size_(size),
        z_(z),
        x_(x),
        y_(y),
        xmin_(xmin),
        ymin_(ymin),
        xmax_(xmax),
        ymax_(ymax) {
        size_ = size;
        latitudes_.reserve(size);
        longitudes_.reserve(size);
        indexes_.reserve(size);
        tiles_.reserve(size);
    }
    void push_back(double lat, double lon, int index) {
        if (lon > xmax_)
            lon -= 360;
        latitudes_.push_back(lat);
        longitudes_.push_back(lon);
        indexes_.push_back(index);
    }
    void push_back(double lat, double lon, int index1, int index2, int index3, int index4, double weight1,
                   double weight2, double weight3, double weight4, double total) {
        if (lon > xmax_)
            lon -= 360;
        latitudes_.push_back(lat);
        longitudes_.push_back(lon);
        tiles_.push_back(PointTile(index1, index2, index3, index4, weight1, weight2, weight3, weight4, total));
    }
    void push_lat(double lat) { latitudes_.push_back(lat); }
    void push_lon(double lon) {
        if (lon > xmax_)
            lon -= 360;
        longitudes_.push_back(lon);
    }
    void dimensions(int rows, int columns) {
        rows_    = rows;
        columns_ = columns;
    }

    vector<double> latitudes_;
    vector<double> longitudes_;
    vector<int> indexes_;
    vector<PointTile> tiles_;
    int size_;
    int z_;
    int x_;
    int y_;
    double xmin_;
    double ymin_;
    double xmax_;
    double ymax_;
    int rows_;
    int columns_;


    void print() {
        cout << "bounding box [" << xmin_ << ", " << xmax_ << "]-->[" << ymin_ << ", " << ymax_ << "]" << endl;
        cout << "Number of points [" << size_ << "]" << endl;
        for (int i = 0; i < size_; i++) {
            cout << "[" << latitudes_[i] << ", " << longitudes_[i] << "]-->" << endl;
        }
    }
};

Tile tile_;

TileDecoder::TileDecoder() {
    cout << "New Tile DEcoder" << endl;
}


bool TileDecoder::ok() {
    string ft = root_ + "/zoom" + tostring(z_) + ".nc";
    file_     = ifstream(ft);
    cout << "Reading tile --> " << ft << endl;
    if (!file_.good()) {
        cout << "can not open " << ft << "return false " << endl;
        return false;
    }
    return true;
}


void TileDecoder::customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out,
                                   bool all) {
#ifdef NOTYET
    if (!file_)
        return;
    int z, x, y, size;
    double lon, lat;
    int index1, index2, index3, index4;
    double weight1, weight2, weight3, weight4, total;
    int index;
    double xmin, ymin, xmax, ymax;
    file_ >> z >> x >> y >> size >> xmin >> ymin >> xmax >> ymax;
    tile_ = Tile(z, x, y, size, xmin, ymin, xmax, ymax);
    /*
    double rows, columns;
    f >> rows >> columns;
    tile_.dimensions(rows, columns);


    for (int i = 0; i < rows; i++) {
        double val;
        f >> val;
        tile_.push_lat(val);
    }

    for (int i = 0; i < columns; i++) {
        double val;
        f >> val;
        tile_.push_lon(val);
    }
    */

    for (int i = 0; i < size; i++) {
        file_ >> lat >> lon >> index;
        tile_.push_back(lat, lon, index);
        /*
        f >> lat >> lon >> index1 >> index2 >> index3 >> index4 >> weight1 >> weight2 >> weight3 >> weight4 >> total;
        tile_.push_back(lat, lon, index1, index2, index3, index4, weight1, weight2, weight3, weight4, total);
        */
    }

    FILE* in = fopen(file_name_.c_str(), "r");
    if (!in) {
        printf("ERROR: unable to open file %s\n", "wind.grib");
        return;
    }
    int error;

    /* create new handle from a message in a file*/
    codes_handle* u = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (u == NULL) {
        printf("Error: unable to create handle from file %s\n", "wind.grib");
    }
    codes_handle* v = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (v == NULL) {
        printf("Error: unable to create handle from file %s\n", "wind.grib");
    }

    /*
        for (auto tile = tile_.tiles_.begin(); tile != tile_.tiles_.end(); ++tile) {
            vector<double> uc;
            uc.reserve(4);
            vector<double> vc;
            vc.reserve(4);
            cout << "compute" << endl;
            for (auto i = tile->indexes_.begin(); i != tile->indexes_.end(); ++i)
                cout << "index " << *i << endl;
            int s = 4;
            codes_get_double_elements(, "values", &(tile->indexes_.front()), s, &uc.front());
            cout << "XXXX" << values.front() << values.size() << endl;
            for (auto i = values.begin(); i != values.end(); ++i)
                cout << "value " << *i << endl;
            cout << tile->compute(values) << endl;
        }
    */

#endif
    Netcdf netcdf("/home/graphics/cgs/proj4/zoom" + tostring(z_) + ".nc", "index");

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

    cout << "Tile-->" << x_ << ", " << y_ << endl;
    int nbpoints = netcdf.getDimension("points");
    netcdf.get("bounding-box", bbox, first, last);
    netcdf.get("index", values, first, last);

    for (auto b = bbox.begin(); b != bbox.end(); ++b)
        cout << "found BBOX" << *b << endl;
    cout << "FOUND DIM " << nbpoints << endl;
    for (auto b = values.begin(); b != values.end(); ++b) {
        double lat = *b;
        ++b;
        double lon = *b;
        ++b;
        double i = *b;

        if (i != -1) {
            latitudes.push_back(lat);

            if (lon > 180)
                longitudes.push_back(lon - 360);
            else
                longitudes.push_back(lon);

            index.push_back(i);
        }
    }
    FILE* in = fopen(file_name_.c_str(), "r");
    if (!in) {
        printf("ERROR: unable to open file %s\n", "wind.grib");
        return;
    }
    int error;

    /* create new handle from a message in a file*/
    codes_handle* u = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (u == NULL) {
        printf("Error: unable to create handle from file %s\n", "wind.grib");
    }
    codes_handle* v = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (v == NULL) {
        printf("Error: unable to create handle from file %s\n", "wind.grib");
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

PointsHandler& TileDecoder::points(const Transformation& t, bool) {
    cout << "HOURAH-->TileDecoder::points" << endl;
}

/*!
 Class information are given to the output-stream.
*/
void TileDecoder::print(ostream& out) const {
    out << "TileDecoder[";
    out << "]";
}

void TileDecoder::decode() {
    tile_.print();

    FILE* in = fopen("wind.grib", "r");
    if (!in) {
        printf("ERROR: unable to open file %s\n", "wind.grib");
        return;
    }
    int error;

    /* create new handle from a message in a file*/
    codes_handle* h = codes_handle_new_from_file(0, in, PRODUCT_GRIB, &error);
    if (h == NULL) {
        printf("Error: unable to create handle from file %s\n", "wind.grib");
    }

    matrix_.set(tile_.rows_, tile_.columns_);

    double missing = -std::numeric_limits<double>::max();

    matrix_.missing(missing);

    for (auto lon = tile_.longitudes_.begin(); lon != tile_.longitudes_.end(); ++lon) {
        cout << "lon -->" << *lon << endl;
        matrix_.columnsAxis().push_back(*lon);
    }

    for (auto lat = tile_.latitudes_.begin(); lat != tile_.latitudes_.end(); ++lat) {
        cout << "lat -->" << *lat << endl;
        matrix_.rowsAxis().push_back(*lat);
    }

    for (auto tile = tile_.tiles_.begin(); tile != tile_.tiles_.end(); ++tile) {
        vector<double> values;
        values.reserve(4);
        cout << "compute" << endl;
        for (auto i = tile->indexes_.begin(); i != tile->indexes_.end(); ++i)
            cout << "index " << *i << endl;
        int s = 4;
        codes_get_double_elements(h, "values", &(tile->indexes_.front()), s, &values.front());
        cout << "XXXX" << values.front() << values.size() << endl;
        for (auto i = values.begin(); i != values.end(); ++i)
            cout << "value " << *i << endl;
        double value = tile->compute(values);
        cout << value << endl;
        matrix_.push_back(value);
    }


    matrix_.setMapsAxis();
}
