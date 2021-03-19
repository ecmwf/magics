/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file NetcdfGeoMatrixInterpretor.cc
    \brief Implementation of the Template class NetcdfGeoMatrixInterpretor.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#include "NetcdfGeoMatrixInterpretor.h"

#include <limits>

#include "ContourLibrary.h"
#include "Factory.h"
#include "Layer.h"
#include "MagicsSettings.h"
#include "NetcdfData.h"

using namespace magics;

NetcdfGeoMatrixInterpretor::NetcdfGeoMatrixInterpretor() {}

NetcdfGeoMatrixInterpretor::~NetcdfGeoMatrixInterpretor() {}

string NetcdfGeoMatrixInterpretor::proj4Detected(Netcdf& netcdf) {
    // Efas old netcdf
    string proj4 = netcdf.getAttribute("projection", string(""));

    if (proj4.size())
        return proj4;


    proj4 = netcdf.getAttribute("gdal_projection", string(""));

    if (proj4.size())
        return proj4;


    string mapping = netcdf.getVariableAttribute(field_, "grid_mapping", string(""));
    if (mapping.size())
        return netcdf.getVariableAttribute(mapping, "proj4_params", string(""));
    return "";
}

bool NetcdfGeoMatrixInterpretor::interpretAsMatrix(Matrix** matrix) {
    if (*matrix)
        return false;

    Netcdf netcdf(path_, dimension_method_);

    string proj4 = proj4Detected(netcdf);


    if (proj4.empty()) {
        matrix_.reset(new Matrix());
        matrix_->akimaEnabled();
    }
    else {
        matrix_.reset(new Proj4Matrix(proj4));
    }
    *matrix = matrix_.get();

    if (automatic_scaling_) {
        WebLibrary settings;
        MetaDataCollector needs;
        settings.askId(needs);
        for (auto need = needs.begin(); need != needs.end(); ++need) {
            need->second = getAttribute(field_, need->first, "");
        }

        settings.getScaling(needs, scaling_, offset_);
    }

    // get the data ...
    try {
        double missing_value = netcdf.getMissing(field_, missing_attribute_);


        if (std::isnan(missing_value)) {
            missing_value = std::numeric_limits<double>::max();
        }

        map<string, string> first, last;
        setDimensions(dimension_, first, last);
        vector<double> inlon, outlon;
        vector<double> inlat, outlat;


        netcdf.get(longitude_, matrix_->columnsAxis(), first, last);


        netcdf.get(latitude_, matrix_->rowsAxis(), first, last);

        matrix_->missing(missing_value);

        if (magCompare(primary_index_, "latitude")) {
            vector<double> data;
            netcdf.get(field_, data, first, last);
            int columns = matrix_->columnsAxis().size();
            int rows    = matrix_->rowsAxis().size();
            for (int lat = 0; lat < rows; lat++) {
                for (int lon = 0; lon < columns; lon++)
                    matrix_->push_back(data[lat + lon * rows]);
            }
        }
        else {
            Timer timer("CREATE matrix", "prepare");
            vector<double> data;
            netcdf.get(field_, data, first, last);
            matrix_->reserve(data.size());
            int i = 0;
            fill(matrix_->begin(), matrix_->end(), missing_value);
            for (vector<double>::iterator d = data.begin(); d != data.end(); ++d) {
                if (!std::isnan(*d) && !std::isinf(*d)) {
                    matrix_->push_back(*d);
                }
                else
                    matrix_->push_back(missing_value);
                i++;
            }
        }
        matrix_->multiply(scaling_);
        matrix_->plus(offset_);
        matrix_->setMapsAxis();
    }
    catch (MagicsException& e) {
        if (MagicsSettings::strict()) {
            throw;
        }
        MagLog::error() << e << "\n";
        matrix_.reset(nullptr);
        return false;
    }
    return true;
}

/*
 Class information are given to the output-stream.
*/
void NetcdfGeoMatrixInterpretor::print(ostream& out) const {
    out << "NetcdfGeoMatrixInterpretor[";
    NetcdfInterpretor::print(out);

    out << "]";
}

UserPoint* NetcdfGeoMatrixInterpretor::newPoint(const string& proj4, double lon, double lat, double val) {
    double x = lon;
    double y = lat;

    if (!projection_.valid()) {
        int error = projection_.revert(x, y);
    }
    return new UserPoint(x, y, val);
}

void NetcdfGeoMatrixInterpretor::visit(Transformation& transformation) {
    // Here are in a dump ode .. the coordinates are pixels.
    if (transformation.getAutomaticX()) {
        transformation.setMinMaxX(matrix_->columnsAxis().front(), matrix_->columnsAxis().back());
    }
    if (transformation.getAutomaticY()) {
        transformation.setMinMaxY(matrix_->rowsAxis().front(), matrix_->rowsAxis().back());
    }
}

bool NetcdfGeoMatrixInterpretor::interpretAsPoints(PointsList& list) {
    Netcdf netcdf(path_, dimension_method_);
    string proj4 = proj4Detected(netcdf);

    if (!proj4.empty()) {
        projection_ = LatLonProjP(proj4);
    }
    // get the data ...
    try {
        vector<double> latitudes;
        vector<double> longitudes;
        vector<double> values;
        map<string, string> first, last;
        setDimensions(dimension_, first, last);
        double missing_value = netcdf.getMissing(field_, missing_attribute_);

        netcdf.get(field_, values, first, last);
        netcdf.get(longitude_, longitudes, first, last);
        netcdf.get(latitude_, latitudes, first, last);
        unsigned int val = 0;

        for (unsigned int lat = 0; lat < latitudes.size(); lat++) {
            for (unsigned int lon = 0; lon < longitudes.size(); lon++) {
                val = (lat * longitudes.size() + lon);
                if (val >= values.size())
                    return true;
                if (values[val] < suppress_below_)
                    continue;
                if (values[val] > suppress_above_)
                    continue;
                if (same(values[val], missing_value))
                    continue;
                list.push_back(newPoint(proj4, longitudes[lon], latitudes[lat], (values[val] * scaling_) + offset_));
            }
        }
        MagLog::dev() << "everything ok" << endl;
    }
    catch (MagicsException& e) {
        if (MagicsSettings::strict()) {
            throw;
        }
        MagLog::error() << e << "\n";
        return false;
    }

    return true;
}

void NetcdfGeoMatrixInterpretor::statsData(map<string, vector<double> >& stats) {
    if (matrix_) {
        for (unsigned int i = 0; i < matrix_->size(); i++) {
            if (matrix_->at(i) != matrix_->missing()) {
                stats["value"].push_back(matrix_->at(i));
            }
        }
    }
}

void NetcdfGeoMatrixInterpretor::visit(MetaDataCollector& mdc) {
    mdc["_datatype"] = "NetCDF_geomatrix";
    mdc["path"]      = path_;
    mdc["latitude"]  = latitude_;
    mdc["longitude"] = longitude_;
    mdc["value"]     = field_;
    mdc["statsType"] = "scalar";

    Netcdf nc(path_, dimension_method_);

    string attrKey;
    string attrVal;

    // Value attributes
    getAttributes(nc, field_, attrKey, attrVal);
    if (!attrKey.empty()) {
        mdc["valueAttrKey"]   = attrKey;
        mdc["valueAttrValue"] = attrVal;
    }
}

void NetcdfGeoMatrixInterpretor::visit(ValuesCollector& vcp, PointsList&) {
    vcp.setCollected(true);

    ASSERT(matrix_);
    const Transformation& transformation = vcp.transformation();
    MatrixHandler* box                   = transformation.prepareData(*matrix_);
    for (ValuesCollector::iterator point = vcp.begin(); point != vcp.end(); ++point) {
        point->push_back(new ValuesCollectorData(point->x(), point->y(), box->nearest(point->y(), point->x()), -1.));
    }
}

void NetcdfGeoMatrixInterpretor::customisedPoints(const Transformation& transformation, const std::set<string>&,
                                                  CustomisedPointsList& out, int thinning) {
    Netcdf netcdf(path_, dimension_method_);

    // get the data ...
    try {
        vector<double> latitudes;
        vector<double> longitudes;
        vector<double> xcomponent;
        vector<double> ycomponent;
        vector<double> colcomponent;
        map<string, string> first, last;
        setDimensions(dimension_, first, last);
        double missing_x = netcdf.getMissing(x_component_, missing_attribute_);
        double missing_y = netcdf.getMissing(y_component_, missing_attribute_);

        netcdf.get(x_component_, xcomponent, first, last);
        netcdf.get(y_component_, ycomponent, first, last);
        if (!colour_component_.empty())
            netcdf.get(colour_component_, colcomponent, first, last);
        netcdf.get(longitude_, longitudes, first, last);
        netcdf.get(latitude_, latitudes, first, last);
        unsigned int val = 0;

        for (unsigned int lat = 0; lat < latitudes.size(); lat += thinning) {
            for (unsigned int lon = 0; lon < longitudes.size(); lon += thinning) {
                val = (lat * longitudes.size() + lon);
                if (val >= xcomponent.size())
                    return;
                if (val >= ycomponent.size())
                    return;
                if (!colour_component_.empty())
                    if (val >= colcomponent.size())
                        return;
                if (same(xcomponent[val], missing_x))
                    continue;
                if (same(ycomponent[val], missing_y))
                    continue;
                vector<UserPoint> points;
                transformation.populate(longitudes[lon], latitudes[lat], 0, points);
                for (vector<UserPoint>::iterator pt = points.begin(); pt != points.end(); ++pt) {
                    CustomisedPoint* point  = new CustomisedPoint(pt->x_, pt->y_, "");
                    (*point)["x_component"] = xcomponent[val];
                    (*point)["y_component"] = ycomponent[val];
                    if (!colour_component_.empty())
                        (*point)["colour_component"] = colcomponent[val];
                    out.push_back(point);
                }
            }
        }
        MagLog::dev() << "everything ok" << endl;
    }
    catch (MagicsException& e) {
        if (MagicsSettings::strict()) {
            throw;
        }
        MagLog::error() << e << "\n";
    }
}

NetcdfInterpretor* NetcdfGeoMatrixInterpretor::guess(const NetcdfInterpretor& from) {
    if (from.field_.empty() && (from.x_component_.empty() || from.y_component_.empty()))
        return 0;

    Netcdf netcdf(from.path_, from.dimension_method_);

    string variable = from.field_;
    if (variable.empty())
        variable = from.x_component_;
    string latitude  = netcdf.detect(variable, "latitude");
    string longitude = netcdf.detect(variable, "longitude");

    if (latitude.size() && longitude.size()) {
        NetcdfGeoMatrixInterpretor* interpretor = new NetcdfGeoMatrixInterpretor();

        interpretor->NetcdfInterpretor::copy(from);
        interpretor->latitude_        = latitude;
        interpretor->longitude_       = longitude;
        interpretor->time_variable_   = netcdf.detect(variable, "time");
        interpretor->level_variable_  = netcdf.detect(variable, "level");
        interpretor->number_variable_ = netcdf.detect(variable, "number");
        return interpretor;
    }

    string projection_y_coordinate = netcdf.detect(variable, "projection_y_coordinate");
    string projection_x_coordinate = netcdf.detect(variable, "projection_x_coordinate");

    if (projection_y_coordinate.size() && projection_y_coordinate.size()) {
        NetcdfGeoMatrixInterpretor* interpretor = new NetcdfGeoMatrixInterpretor();
        interpretor->NetcdfInterpretor::copy(from);

        if (interpretor->proj4Detected(netcdf).size()) {
            interpretor->latitude_        = projection_y_coordinate;
            interpretor->longitude_       = projection_x_coordinate;
            interpretor->time_variable_   = netcdf.detect(variable, "time");
            interpretor->level_variable_  = netcdf.detect(variable, "level");
            interpretor->number_variable_ = netcdf.detect(variable, "number");
            return interpretor;
        }
    }

    return 0;
}
