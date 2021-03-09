/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BinningObject.cc
    \brief Implementation of the Template class BinningObject.

    Magics Team - ECMWF 2011

    Started: Thu 7-Apr-2011

    Changes:

*/

#include "BinningObject.h"
#include <algorithm>

using namespace magics;


BinningObject::BinningObject() {
    binners_x_["count"]    = &BinningObject::countx;
    binners_x_["list"]     = &BinningObject::listx;
    binners_x_["interval"] = &BinningObject::intervalx;

    binners_y_["count"]    = &BinningObject::county;
    binners_y_["list"]     = &BinningObject::listy;
    binners_y_["interval"] = &BinningObject::intervaly;
}

BinningObject::~BinningObject() {}

/*!
 Class information are given to the output-stream.
*/
void BinningObject::print(ostream& out) const {
    out << "BinningObject[";
    BinningObjectAttributes::print(out);
    out << "]";
}

void BinningObject::build(vector<double>& vals, IntervalMap<int>& binns) {
    vector<double>::iterator from = vals.begin();
    vector<double>::iterator to   = vals.begin();
    ++to;
    int index = 0;
    double min, max;
    min = *from;
    while (to != vals.end()) {
        max = (*from + *to) / 2;

        binns.insert(make_pair(Interval(min, max), index));
        min = max;
        index++;
        ++to;
        ++from;
    }
    max = vals.back();
    binns.insert(make_pair(Interval(min, max), index));
}

/*


*/
static double mindef = -1.0e+21;
static double maxdef = 1.0e+21;

void BinningObject::countx(vector<double>& vals, double min, double max) {
    const double vmin = (x_min_ == mindef) ? min : std::max(min, x_min_);
    const double vmax = (x_max_ == maxdef) ? max : std::min(max, x_max_);
    const double step = (max - min) / (x_count_);

    for (double val = vmin; val < vmax; val += step) {
        vals.push_back(val);
    }

    vals.push_back(vmax + EPSILON);
}

void BinningObject::county(vector<double>& vals, double min, double max) {
    const double vmin = (y_min_ == mindef) ? min : std::max(min, y_min_);
    const double vmax = (y_max_ == maxdef) ? max : std::min(max, y_max_);
    const double step = (max - min) / (y_count_);

    for (double val = vmin; val < vmax; val += step) {
        vals.push_back(val);
    }

    vals.push_back(vmax + EPSILON);
}

void BinningObject::listx(vector<double>& vals, double min, double max) {
    const double vmin = (y_min_ == mindef) ? min : std::max(min, y_min_);
    const double vmax = (y_max_ == maxdef) ? max : std::min(max, y_max_);

    for (vector<double>::iterator val = x_list_.begin(); val != x_list_.end(); ++val) {
        if (*val >= vmin && *val <= vmax)
            vals.push_back(*val);
    }
}

void BinningObject::listy(vector<double>& vals, double min, double max) {
    const double vmin = (y_min_ == mindef) ? min : std::max(min, y_min_);
    const double vmax = (y_max_ == maxdef) ? max : std::min(max, y_max_);

    for (vector<double>::iterator val = x_list_.begin(); val != x_list_.end(); ++val) {
        if (*val >= vmin && *val <= vmax)
            vals.push_back(*val);
    }
}

void BinningObject::intervalx(vector<double>& vals, double min, double max) {
    const double vmin = (x_min_ == mindef) ? min : std::max(min, x_min_);
    const double vmax = (x_max_ == maxdef) ? max : std::min(max, x_max_);

    for (double val = x_reference_; val < vmax; val += x_interval_)
        vals.push_back(val);
    for (double val = x_reference_ - x_interval_; val > vmin; val -= x_interval_)
        vals.push_back(val);

    std::sort(vals.begin(), vals.end());
    vals.insert(vals.begin(), vals.front() - x_interval_);
    vals.push_back(vals.back() + x_interval_);
}

void BinningObject::intervaly(vector<double>& vals, double min, double max) {
    const double vmin = (y_min_ == mindef) ? min : std::max(min, y_min_);
    const double vmax = (y_max_ == maxdef) ? max : std::min(max, y_max_);

    for (double val = y_reference_; val < vmax; val += y_interval_)
        vals.push_back(val);

    for (double val = y_reference_ - y_interval_; val > vmin; val -= y_interval_)
        vals.push_back(val);

    std::sort(vals.begin(), vals.end());
    vals.insert(vals.begin(), vals.front() - y_interval_);
    vals.push_back(vals.back() + y_interval_);
}

Matrix* BinningObject::operator()(PointsList& points) {
    Matrix* matrix = new Matrix();

    double minx = points.minX();
    double maxx = points.maxX();
    double miny = points.minY();
    double maxy = points.maxY();
    double max  = points.min();
    double min  = points.max();

    IntervalMap<int> xbinns;
    IntervalMap<int> ybinns;
    x_                                   = lowerCase(x_);
    y_                                   = lowerCase(y_);
    map<string, binner>::iterator binner = binners_x_.find(x_);

    if (binner != binners_x_.end()) {
        (this->*binner->second)(matrix->columnsAxis(), minx, maxx);
        if (matrix->columnsAxis().empty()) {
            MagLog::warning() << " could not find any binns: return to count method" << endl;
            countx(matrix->columnsAxis(), minx, maxx);
        }
    }
    else {
        MagLog::warning() << " could not find the method " << x_ << " for binning: return to count method" << endl;
        countx(matrix->columnsAxis(), minx, maxx);
    }
    build(matrix->columnsAxis(), xbinns);
    binner = binners_y_.find(y_);

    if (binner != binners_y_.end()) {
        (this->*binner->second)(matrix->rowsAxis(), miny, maxy);
        if (matrix->rowsAxis().empty()) {
            MagLog::warning() << " could not find any binns: return to count method" << endl;
            county(matrix->columnsAxis(), minx, maxx);
        }
    }
    else {
        MagLog::warning() << " could not find the method " << y_ << " for binning: return to count method" << endl;
        county(matrix->rowsAxis(), miny, maxy);
    }
    build(matrix->rowsAxis(), ybinns);


    matrix->setMapsAxis();
    //		double val = 0;
    vector<double> total;

    for (int j = 0; j < matrix->columns(); j++)
        for (int i = 0; i < matrix->rows(); i++) {
            matrix->push_back(0);
            total.push_back(0);
        }

    double columns = matrix->columns();

    points.setToFirst();
    while (points.more()) {
        const UserPoint& point = points.current();
        int x                  = xbinns.find(point.x_, -1);
        int y                  = ybinns.find(point.y_, -1);

        if (x != -1 && y != -1) {
            (*matrix)[y * columns + x] = (*matrix)[y * columns + x] + 1;
            total[y * columns + x]     = total[y * columns + x] + point.value();
        }
        points.advance();
    }

    if (min != max) {
        for (unsigned int i = 0; i < matrix->size(); ++i) {
            if ((*matrix)[i])
                (*matrix)[i] = total[i] / (*matrix)[i];
            else {
                (*matrix)[i] = matrix->missing();
            }
        }
    }
    /*
    for ( int c = 0; c < columns; ++c) {
                        MagLog::dev() << "[" << matrix->row(r,c) << ",  " << matrix->column(r,c) << "] = " <<
    (*matrix)(r, c) << endl;
                    }
        }i*/
    return matrix;
}


NoBinningObject::NoBinningObject() {
}

NoBinningObject::~NoBinningObject() {

}

BinningObject* NoBinningObject::clone() const {
    return new NoBinningObject();
}

Matrix* NoBinningObject::operator()(PointsList& points) {

    static float default_float_fill_value = 9.96921e+36;

    // make a new Matrix object
    Matrix* ret = new Matrix();

    ret->akimaEnabled();
    ret->reserve(points.size());
    ret->missing(default_float_fill_value);

    // set column values (longitudes)
    points.setToFirst();

    double last_latitude = -360;

    vector<double> &xValues = ret->columnsAxis();
    while (points.more()) {
        const UserPoint& current_point = points.current();
        double current_lat = current_point.y();

        if ( (last_latitude != -360) && (last_latitude != current_lat))
            break;

        last_latitude = current_lat;
        xValues.push_back(current_point.x());
        points.advance();
    }

    int columns = xValues.size();
    int rows    = points.size() / columns;

    // set row (latitude) and data values
    points.setToFirst();

    int row    = points.size() - columns;
    int column = 0;

    vector<double> &yValues = ret->rowsAxis();
    while (points.more()) {
        const UserPoint& current_point = points.current();
        double current_value = current_point.value();

        (*ret)[row + column] = current_value;

        points.advance();
        ++column;

        if (column == columns) {
            row -= columns;
            column = 0;
            yValues.insert(yValues.begin(), current_point.y());
        }
    }

    ret->setMapsAxis();

    return ret;
}
