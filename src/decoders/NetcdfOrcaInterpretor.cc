/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file NetcdfOrcaInterpretor.h
    \brief Implementation of the Template class NetcdfOrcaInterpretor.

    Magics Team - ECMWF 2004

    Started: Tue 17-Feb-2004

    Changes:

*/

#include "NetcdfOrcaInterpretor.h"

#include <limits>

#include "Factory.h"
#include "Layer.h"
#include "NetcdfData.h"
#include "SciMethods.h"

using namespace magics;

NetcdfOrcaInterpretor::NetcdfOrcaInterpretor() : matrix_(0) {}

NetcdfOrcaInterpretor::~NetcdfOrcaInterpretor() {}

bool NetcdfOrcaInterpretor::interpretAsMatrix(Matrix** data) {
  if (*data) return false;

  Netcdf netcdf(path_, dimension_method_);
  NetVariable var = netcdf.getVariable(longitude_);
  vector<size_t> dims;
  var.getDimensions(dims);

  int jin = dims[dims.size() - 2];
  int iin = dims[dims.size() - 1];
  int jout = jin;
  int iout = iin;

  matrix_ = new Matrix(jout, iout);

  *data = matrix_;

  double missing = netcdf.getMissing(field_, missing_attribute_);
  typedef pair<int, int> point_type;

  vector<std::pair<point_type, pair<int, int> > > points;

  matrix_->missing(missing);
  // get the data ...
  try {
    Timer time("orca", "decode");
    map<string, string> first, last;
    setDimensions(dimension_, first, last);

    MagLog::debug() << "data[" << matrix_->size() << ":"
                    << *std::min_element(matrix_->begin(), matrix_->end())
                    << ", " << offset_ << "\n";

    vector<double> latm;
    vector<double> lonm;
    vector<double> data;

    netcdf.get(longitude_, lonm, first, last);
    netcdf.get(latitude_, latm, first, last);
    netcdf.get(field_, data, first, last);

    double minlat = *std::min_element(latm.begin(), latm.end());
    double maxlat = *std::max_element(latm.begin(), latm.end());

    double minlon = *std::min_element(lonm.begin(), lonm.end());
    double maxlon = *std::max_element(lonm.begin(), lonm.end());

    vector<double>& lon = matrix_->columnsAxis();
    vector<double>& lat = matrix_->rowsAxis();

    // for the lon we take the fisrt line :
    double inci = (maxlon - minlon) / ((iout)-1);
    double incj = (maxlat - minlat) / ((jout)-1);
    for (int i = 0; i < iout; i++) lon.push_back(minlon + (i * inci));
    for (int i = 0; i < jout; i++) lat.push_back(minlat + (i * incj));
    for (int y = 0; y < lat.size(); ++y) {
      for (int x = 0; x < lon.size(); ++x) {
        (*matrix_)[x + (y * iout)] = missing;
      }
    }

    double lat11, lat12, lat21, lat22;
    double lon11, lon12, lon21, lon22;
    double val11, val12, val21, val22;

    for (int r = 0; r < jin - 1; r++) {
      for (int c = 0; c < iin - 1; c++) {
        lat11 = latm[c + (iin * r)];
        lat12 = latm[(c + 1) + (iin * r)];
        lat21 = latm[c + (iin * (r + 1))];
        lat22 = latm[(c + 1) + (iin * (r + 1))];

        minlat = std::min(lat11, lat12);
        maxlat = std::max(lat11, lat12);
        minlat = std::min(minlat, lat21);
        maxlat = std::max(maxlat, lat21);
        minlat = std::min(minlat, lat22);
        maxlat = std::max(maxlat, lat22);

        lon11 = lonm[c + (iin * r)];
        lon12 = lonm[(c + 1) + (iin * r)];
        lon21 = lonm[c + (iin * (r + 1))];
        lon22 = lonm[(c + 1) + (iin * (r + 1))];

        val11 = data[c + (iin * r)];
        val12 = data[(c + 1) + (iin * r)];
        val21 = data[c + (iin * (r + 1))];
        val22 = data[(c + 1) + (iin * (r + 1))];

        minlon = std::min(lon11, lon12);
        maxlon = std::max(lon11, lon12);
        minlon = std::min(minlon, lon21);
        maxlon = std::max(maxlon, lon21);
        minlon = std::min(minlon, lon22);
        maxlon = std::max(maxlon, lon22);

        bool shift = false;

        if ((maxlon - minlon) > 360 - (maxlon - minlon)) {
          shift = true;
          // we move all the negative
          double l11 = lon11 < 0 ? lon11 + 360 : lon11;
          double l12 = lon12 < 0 ? lon12 + 360 : lon12;
          double l21 = lon21 < 0 ? lon21 + 360 : lon21;
          double l22 = lon22 < 0 ? lon22 + 360 : lon22;
          minlon = std::min(l11, l12);
          maxlon = std::max(l11, l12);
          minlon = std::min(minlon, l21);
          maxlon = std::max(maxlon, l21);
          minlon = std::min(minlon, l22);
          maxlon = std::max(maxlon, l22);
        }

        // Now we fill the matrix
        for (int y = 0; y < lat.size(); ++y) {
          if (lat[y] < minlat) continue;
          if (lat[y] > maxlat) break;

          for (int x = 0; x < lon.size(); ++x) {
            double l = (shift && lon[x] < 0) ? lon[x] + 360 : lon[x];
            if (l < minlon) continue;
            if (l > maxlon + inci) {
              break;
            }

            double val = missing;
            double d11 = geoDistanceInKm(lon11, lat11, lon[x], lat[y]);
            double d12 = geoDistanceInKm(lon12, lat12, lon[x], lat[y]);
            double d21 = geoDistanceInKm(lon21, lat21, lon[x], lat[y]);
            double d22 = geoDistanceInKm(lon22, lat22, lon[x], lat[y]);
            if (val11 == missing && val12 == missing && val21 == missing &&
                val22 == missing) {
              val = missing;
            } else {
              if (val11 == missing) val11 = 0;
              if (val12 == missing) val12 = 0;
              if (val21 == missing) val21 = 0;
              if (val22 == missing) val22 = 0;

              val = ((d11 * val11) + (d12 * val12) + (d21 * val21) +
                     (d22 * val22)) /
                    (d11 + d22 + d21 + d12);
            }
            if (val != missing) (*matrix_)[x + (y * iout)] = val;
          }
        }
      }

      matrix_->multiply(scaling_);
      matrix_->plus(offset_);
      matrix_->setMapsAxis();

      MagLog::dev() << *matrix_ << "\n";
    }
  }

  catch (MagicsException& e) {
    MagLog::error() << e << "\n";
    return false;
  }
  return true;
}

bool NetcdfOrcaInterpretor::interpretAsPoints(PointsList& points) {
  // later!

  // get the data ...
  try {
    MagLog::dev() << " Netcdf File Path --->" << path_ << "\n";
    Netcdf netcdf(path_, dimension_method_);
    map<string, string> first, last;
    setDimensions(dimension_, first, last);
    double missing = netcdf.getMissing(field_, missing_attribute_);
    vector<double> latm;
    vector<double> lonm;
    vector<double> data;

    netcdf.get(longitude_, lonm, first, last);
    netcdf.get(latitude_, latm, first, last);
    netcdf.get(field_, data, first, last);

    vector<double>::iterator lat = latm.begin();
    vector<double>::iterator lon = lonm.begin();
    vector<double>::iterator val = data.begin();

    while (lat != latm.end()) {
      double value = *val;
      if (std::isnan(value)) value = missing;

      if (value != missing) {
        value = (value * scaling_) + offset_;
        points.push_back(new UserPoint(*lon, *lat, value));
      }
      ++lat;
      ++lon;
      ++val;
    }
  }

  catch (MagicsException& e) {
    MagLog::error() << e << "\n";
  }
  return true;
}

void NetcdfOrcaInterpretor::visit(ValuesCollector& vcp, PointsList&) {
  vcp.setCollected(true);

  if (matrix_) {
    const Transformation& transformation = vcp.transformation();
    MatrixHandler* box = transformation.prepareData(*matrix_);
    for (ValuesCollector::iterator point = vcp.begin(); point != vcp.end();
         ++point) {
      point->push_back(new ValuesCollectorData(
          point->x(), point->y(), box->nearest(point->y(), point->x()), -1.));
    }
  }
}

void NetcdfOrcaInterpretor::customisedPoints(
    const Transformation& transformation, const std::set<string>&,
    CustomisedPointsList& out, int thinning) {
  try {
    MagLog::dev() << " Netcdf File Path --->" << path_ << "\n";
    Netcdf netcdf(path_, dimension_method_);
    map<string, string> first, last;
    setDimensions(dimension_, first, last);
    double missing = netcdf.getMissing(field_, missing_attribute_);
    vector<double> latitudes;
    vector<double> longitudes;
    vector<double> x_component, y_component;

    netcdf.get(longitude_, longitudes, first, last);
    netcdf.get(latitude_, latitudes, first, last);
    netcdf.get(x_component_, x_component, first, last);
    netcdf.get(y_component_, y_component, first, last);

    for (int ind = 0; ind < latitudes.size(); ind += thinning) {
      CustomisedPoint* point = new CustomisedPoint();
      point->longitude(longitudes[ind]);
      point->latitude(latitudes[ind]);
      (*point)["x_component"] = x_component[ind];
      (*point)["y_component"] = y_component[ind];
      out.push_back(point);
    }
  }

  catch (MagicsException& e) {
    MagLog::error() << e << "\n";
  }
}

/*!
 Class information are given to the output-stream.
*/
void NetcdfOrcaInterpretor::print(ostream& out) const {
  out << "NetcdfOrcaInterpretor[";
  NetcdfInterpretor::print(out);

  out << "]";
}

NetcdfInterpretor* NetcdfOrcaInterpretor::guess(const NetcdfInterpretor& from) {
  if (from.field_.empty() &&
      (from.x_component_.empty() || from.y_component_.empty()))
    return 0;

  Netcdf netcdf(from.path_, from.dimension_method_);

  string variable = from.field_;

  // get the attribute coordinates

  string coordinates =
      netcdf.getVariable(variable).getAttribute("coordinates", string(""));
  string latlon("lat lon");
  coordinates = coordinates.substr(0, latlon.size());

  cout << "COORDINATES" << coordinates << endl;

  if (coordinates == "lat lon" || coordinates == "lon lat") {
    NetcdfOrcaInterpretor* interpretor = new NetcdfOrcaInterpretor();
    cout << "FOUND ORCA " << endl;

    interpretor->NetcdfInterpretor::copy(from);
    interpretor->latitude_ = "lat";
    interpretor->longitude_ = "lon";

    interpretor->time_variable_ = netcdf.detect(variable, "time");
    interpretor->level_variable_ = netcdf.detect(variable, "level");
    interpretor->number_variable_ = netcdf.detect(variable, "number");
    return interpretor;
  }
  return 0;
}

static SimpleObjectMaker<NetcdfOrcaInterpretor, NetcdfInterpretor>
    netcdf_geovalues_interpretor("orca");
