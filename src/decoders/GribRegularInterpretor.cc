/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file GribRegularInterpretor.cc
 \brief Implementation of the Template class GribRegularInterpretor.
 \author Meteorological Visualisation Section, ECMWF

 Started: Mon 18-Apr-2005

 Changes:
 */

#include <limits>

#include "CustomisedPoint.h"
#include "TeProjection.h"
#include "GribRegularInterpretor.h"
#include "GribDecoder.h"
#include "Matrix.h"
#include "LocalTable.h"
#include "RasterData.h"
#include "Timer.h"

using namespace magics;

GribRegularInterpretor::GribRegularInterpretor() {
}

GribRegularInterpretor::~GribRegularInterpretor() {
}

void GribInterpretor::longitudesSanityCheck(double& west, double& east) const {
	// make sure that the west longitudes is always inferior to teh East longitudes and always
	// betwwen -180 and 360...

	while (east <= west) {
		// We add 360 to the east ...
		east += 360;
	}

	// We reposition if needed 

	while (east > 360) {
		west -= 360.;
		east -= 360.;
	}
}
void GribInterpretor::scaling(const GribDecoder& grib, double& scaling,
		double& offset) const {
	string originalUnits, derivedUnits;
	this->scaling(grib, scaling, offset, originalUnits, derivedUnits);
}

void GribInterpretor::scaling(const GribDecoder& grib, double& scaling,
		double& offset, string& originalUnits, string& derivedUnits) const {
	scaling = 1;
	offset = 0;

	// First check that they are not derived fields! 

	if (grib.scaling_ || grib.derived_scaling_) {
		long derived = grib.getLong("generatingProcessIdentifier");

		if ((derived != 254 && grib.scaling_)
				|| (derived == 254 && grib.derived_scaling_)) {
			// The key 'paramId' embodies a number of features such as centre, parameter,
			// level, etc. This means that we should not need to worry about table
			// numbers, etc as we did with GRIBEX. However, it's not entirely clear
			// what we can do with non-ECMWF data, as we don't seem to have tables
			// giving scaling factors & offsets for such data. The code, as originally
			// written here takes a table-based approach, but this will probably
			// become redundant. What we really need is something more based on paramIds.
			// In the meantime, we will take the paramIds as if they are from ECMWF data.
			// In practice, this means pretending that all data is from
			// centre=98,table=128.

			long table = 128; // hard-coded in case of GRIB 2
			long centre = 98;  // hard-coded in case of GRIB 2

			//long edition = grib.getLong("edition");
			//
			//if (edition == 1)
			//{
			//	table  = grib.getLong("table2Version");
			//	centre = grib.getLong("centre");
			//}
			//else
			//{
			//	// GRIB 2 does not use the table-based approach, so we just hope that this will
			//	// work with most data if we use the standard ECMWF tables...
			//}

			long id = grib.getLong("paramId");

			try {
				const ParamDef& paramdef = LocalTable::localInfo(id, table,
						centre);
				scaling = paramdef.scaling();
				offset = paramdef.offset();
				originalUnits = paramdef.originalUnit();
				derivedUnits = paramdef.derivedUnit();
			} catch (...) {
				MagLog::warning()
						<< " Can not find information for the parameter [" << id
						<< "." << table << "]\n";
			}
		}
	} else {
		scaling = grib.scaling_factor_;
		offset = grib.scaling_offset_;
	}
	// Add a sanity check : the factor can not be 0..
	if (scaling == 0)
		scaling = 1;
}

void GribInterpretor::scaling(const GribDecoder& grib, Matrix** matrix) const {
	double factor, offset;

	scaling(grib, factor, offset);

	(*matrix)->multiply(factor);
	(*matrix)->plus(offset);
}

void GribInterpretor::raw(GribDecoder& grib,
		const Transformation& transformation,
		vector<pair<double, vector<pair<double, CustomisedPoint*> > > >& points,
		double& minlon, double& maxlon) const
{
	Timer timer("grib", "raw");
	double factor, offset;
	scaling(grib, factor, offset);
	int err;

	string uname, vname;

	grib_handle* uc = grib.uHandle(uname);
	grib_handle* vc = grib.vHandle(vname);

	long nblon = grib.getLong("numberOfPointsAlongAParallel");
	long nblat = grib.getLong("numberOfPointsAlongAMeridian");

	points.reserve(nblat);

	grib_iterator* uiter = grib_iterator_new(uc, 0, &err);
	grib_iterator* viter = grib_iterator_new(vc, 0, &err);
	double missing = grib.getDouble("missingValue");

	double lat, lon, u, v;
	double last = -99999999;

	minlon = 999999;
	maxlon = -999999;

	while (grib_iterator_next(uiter, &lat, &lon, &u)
			&& grib_iterator_next(viter, &lat, &lon, &v)) {

		if (transformation.in(lon, lat) || transformation.in(lon - 360, lat)) {
			if (minlon > lon)
				minlon = lon;
			if (maxlon < lon)
				maxlon = lon;
			if (u != missing)
				u = (u * factor) + offset;
			if (v != missing)
				v = (v * factor) + offset;

			if (lat != last) {
				last = lat;
				points.push_back(
						make_pair(lat,
								vector<pair<double, CustomisedPoint*> >()));
				points.back().second.reserve(nblat * 2);

			}

			CustomisedPoint* point = new CustomisedPoint(lon, lat, "");
			point->insert(make_pair(uname, u));
			point->insert(make_pair(vname, v));
			points.back().second.push_back(make_pair(lon, point));

		}

	}

	grib_iterator_delete(viter);
	grib_iterator_delete(uiter);

	for (vector<pair<double, vector<pair<double, CustomisedPoint*> > > >::iterator ilat =
			points.begin(); ilat != points.end(); ++ilat) {
		vector < pair<double, CustomisedPoint*> > &lons = ilat->second;

		CustomisedPoint* first = lons.front().second;
		;
		CustomisedPoint* last = lons.back().second;
		//if (last->longitude() - first->longitude() > XResolution(grib)) {
		CustomisedPoint* dup = new CustomisedPoint(first->longitude() + 360,first->latitude(), "");
		dup->insert(make_pair("x_component", (*first)["x_component"]));
		dup->insert(make_pair("y_component", (*first)["y_component"]));
		lons.push_back(make_pair(dup->longitude(), dup));
		//}

	}

}

void GribInterpretor::raw(const GribDecoder& grib,
		const Transformation& transformation, const string& key,
		map<double, map<double, CustomisedPoint*> >& points) const {
	Timer timer("grib", "raw");
	double factor, offset;
	scaling(grib, factor, offset);
	int err;
	size_t nb;
	grib_get_size(grib.id(), "values", &nb);

	map<double, map<double, CustomisedPoint*> >::iterator ilat;
	map<double, CustomisedPoint*>::iterator ilon;

	grib_iterator* iter = grib_iterator_new(grib.handle(), 0, &err);
	double missing = grib.getDouble("missingValue");

	double lat, lon, value;

	/* Loop on all the lat/lon/values. */
	while (grib_iterator_next(iter, &lat, &lon, &value)) {

		if (transformation.in(lon, lat) || transformation.in(lon - 360, lat)) {
			if (value != missing)
				value = (value * factor) + offset;

			ilat = points.find(lat);
			if (ilat == points.end()) {

				points.insert(make_pair(lat, map<double, CustomisedPoint*>()));
				ilat = points.find(lat);

			}
			ilon = ilat->second.find(lon);

			if (ilon == ilat->second.end()) {
				CustomisedPoint* pt = new CustomisedPoint(lon, lat, "");
				pt->missing(true);
				ilat->second.insert(make_pair(lon, pt));
				ilon = ilat->second.find(lon);

			}
			ilon->second->insert(make_pair(key, value));

		}

	}

	/* At the end the iterator is deleted to free memory. */
	grib_iterator_delete(iter);

}
/*!
 Class information are given to the output-stream.
 */
void GribRegularInterpretor::print(ostream& out) const {
	out << "GribRegularInterpretor[";
	out << "]";
}

void GribRegularInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix, const Transformation&) const {
	interpretAsMatrix(grib, matrix);
}

void GribRegularInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {
	Timer timer("gribapi", " read grib");
	MagLog::dev() << "GribRegularInterpretor::interpretAsMatrix" << "\n";
	long nblon = grib.getLong("numberOfPointsAlongAParallel");
	long nblat = grib.getLong("numberOfPointsAlongAMeridian");

	if (*matrix == 0)
		*matrix = new Matrix(nblat, nblon);

	size_t nb;
	grib_get_size(grib.id(), "values", &nb);

	MagLog::dev() << "numberOfFieldValues[" << nb << "]" << "\n";
	double missing = INT_MAX;
	grib.setDouble("missingValue", missing);
	(*matrix)->missing(missing);
	(*matrix)->akimaEnabled();

	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double south = grib.getDouble("latitudeOfLastGridPointInDegrees");
	;
	double east = grib.getDouble("longitudeOfLastGridPointInDegrees");
	;
	longitudesSanityCheck(west, east);

	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << ", " << east
			<< ", " << south << "]" << "\n";
	double loni = longitudeIncrement(grib);

	double lon = (east - west) / (nblon - 1);

	MagLog::dev() << "increment -->" << loni << " (from->" << west << " to-->"
			<< west + (nblon - 1) * loni << ")" << endl;
	MagLog::dev() << "calcul -->" << lon << " (from->" << west << " to-->"
			<< west + (nblon - 1) * lon << ")" << endl;

	latitudes(grib, (*matrix)->rowsAxis());

	double x = west;
	for (int i = 0; i < nblon; i++) {
		(*matrix)->columnsAxis().push_back(x);
		x = west + (i + 1) * lon;
	}

	(*matrix)->setMapsAxis();

	long jPointsAreConsecutive = grib.getLong("jPointsAreConsecutive");

	try {
		(*matrix)->resize(nb);
		size_t aux = size_t(nb);

		// if jPointsAreConsecutive=1 then the values represent columns of data instead
		// of rows, so we have to 'reshape' the array so that it is reorganised into rows.

		if (jPointsAreConsecutive) {
			vector<double> *d = new vector<double>(nb);  // temporary array
			double *d1 = &d->front();                 // temporary array pointer
			double *d2 = &(*matrix)->front();            // final array

			grib_get_double_array(grib.id(), "values", d1, &aux);

			for (int i = 0; i < nblon; i++) {
				for (int j = 0; j < nblat; j++) {
					d2[j * nblon + i] = d1[i * nblat + j];
				}
			}

			delete d;
		} else  // otherwise, just copy the array of values as they are
		{
			grib_get_double_array(grib.id(), "values", &(*matrix)->front(),
					&aux);
		}

		(*matrix)->missing(missing);

	} catch (...) {
		throw MagicsException("GribRegularInterpretor - Not enough memory");
	}
}

void GribRegularInterpretor::latitudes(const GribDecoder& grib,
		vector<double>& latitudes) const {
	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");

	long nblat = grib.getLong("numberOfPointsAlongAMeridian");
	int scanning = grib.getLong("jScansPositively") ? 1 : -1;
	double lat = scanning * grib.getDouble("jDirectionIncrementInDegrees");

	double y = north;
	for (int i = 0; i < nblat; i++) {
		latitudes.push_back(y);
		y += lat;
	}

}

void GribRegularGaussianInterpretor::latitudes(const GribDecoder& grib,
		vector<double>& latitudes) const {
	long res = grib.getLong("numberOfParallelsBetweenAPoleAndTheEquator");
	double array[2 * res];
	grib_get_gaussian_latitudes(res, array);
	double first = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double last = grib.getDouble("latitudeOfLastGridPointInDegrees");
	;
	// get the scanning mode !
	double north = first;
	double south = last;
	long scanning = grib.getLong("jScansPositively");
	;
	if (scanning == 1) {
		north = last;
		south = first;
	}

	for (int i = 0; i < 2 * res; i++) {
		if (same(array[i], north, 10e-2)) {
			latitudes.push_back(array[i]);
			continue;
		}
		if (same(array[i], south, 10e-2)) {
			latitudes.push_back(array[i]);
			continue;
		}
		if (array[i] < north && array[i] > south)
			latitudes.push_back(array[i]);
	}

	if (scanning == 1)
		std::reverse(latitudes.begin(), latitudes.end());
}

double GribRegularInterpretor::longitudeIncrement(
		const GribDecoder& grib) const {
	int scanning = grib.getLong("iScansNegatively") ? -1 : 1;
	return scanning * grib.getDouble("iDirectionIncrementInDegrees");

}

PaperPoint GribInterpretor::reference(const GribDecoder& grib, const Transformation& transformation)
{
	double lon = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double lat = grib.getDouble("latitudeOfFirstGridPointInDegrees");

	UserPoint point(grib.getDouble("longitudeOfFirstGridPointInDegrees"),
			grib.getDouble("latitudeOfFirstGridPointInDegrees"));

	if (transformation.in(point))
		return transformation(point);
	int err;
	grib_iterator* iter = grib_iterator_new(grib.handle(), 0, &err);

	double value;
	/* Loop on all the lat/lon/values. */
	while (grib_iterator_next(iter, &lat, &lon, &value)) {
		point = UserPoint(lon, lat);
		if (transformation.in(point)) {
			grib_iterator_delete(iter);
			return transformation(point);
		}
	}

	/* At the end the iterator is deleted to free memory. */
	grib_iterator_delete(iter);
	return PaperPoint(transformation.getMinPCX(), transformation.getMinPCY());
}

void GribRegularInterpretor::interpretAsRaster(const GribDecoder& grib,
		RasterData& raster, const Transformation& transformation) const {
	MagLog::dev() << "GribRegularInterpretor::interpretAsRaster" << "\n";

	BoxMatrixHandler box(const_cast<GribDecoder*>(&grib)->matrix(),
			transformation);

	int nblon = box.columns();
	int nblat = box.rows();
	double east = box.column(0, nblon - 1);
	double west = box.column(0, 0);
	double south = box.row(0, 0);
	double north = box.row(nblat - 1, 0);

	raster.setUpperRightCorner(east, north);
	raster.setLowerLeftCorner(west, south);

	double lon = (east - west) / (nblon - 1);
	double lat = (north - south) / (nblat - 1);

	raster.setXResolution(lon);
	raster.setYResolution(lat);

	raster.setColumns(nblon);
	raster.setRows(nblat);
	//
	raster.setProjection(new TeLatLong(TeDatum()));

	raster.reserve(nblon * nblat);

	for (int j = nblat - 1; j >= 0; j--)
		for (int i = 0; i < nblon; i++)
			raster.push_back(box(j, i));
}

void GribReducedGaussianInterpretor::interpretAsRaster(const GribDecoder& grib,
		RasterData& raster, const Transformation& transformation) const {
	MagLog::dev() << "GribRegularInterpretor::interpretAsRaster" << "\n";
	Timer timer("grib api", "read grib");
	BoxMatrixHandler box(const_cast<GribDecoder*>(&grib)->matrix(),
			transformation);

	int nblon = box.columns();
	int nblat = box.rows();
	double east = box.column(0, nblon - 1);
	double west = box.column(0, 0);
	double south = box.row(0, 0);
	double north = box.row(nblat - 1, 0);

	raster.setUpperRightCorner(east, north);
	raster.setLowerLeftCorner(west, south);

	double lon = (east - west) / (nblon - 1);
	double lat = (north - south) / (nblat - 1);

	raster.setXResolution(lon);
	raster.setYResolution(lat);

	raster.setColumns(nblon);
	raster.setRows(nblat);
	//
	raster.setProjection(new TeLatLong(TeDatum()));

	raster.reserve(nblon * nblat);

	for (int j = nblat - 1; j >= 0; j--)
		for (int i = 0; i < nblon; i++)
			raster.push_back(box(j, i));
}

void GribReducedGaussianInterpretor::print(ostream& out) const {
	out << "GribRegularInterpretor[";
	out << "]";
}

void GribReducedGaussianInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix, const Transformation& transformation) const {
	interpretAsMatrix(grib, matrix);

}

double GribReducedLatLonInterpretor::XResolution(
		const GribDecoder& grib) const {
	long res = grib.getLong("Nj");

	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double east = grib.getDouble("longitudeOfLastGridPointInDegrees");
	;

	longitudesSanityCheck(west, east);

	return (east - west) / (2 * res);
}
double GribReducedGaussianInterpretor::XResolution(
		const GribDecoder& grib) const {
	long res = grib.getLong("numberOfParallelsBetweenAPoleAndTheEquator");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");

	double east = grib.getDouble("longitudeOfLastGridPointInDegrees");
	;

	longitudesSanityCheck(west, east);
	return (east - west) / (4 * res);
}

void GribReducedGaussianInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {
	MagLog::dev() << "GribRegularInterpretor::interpretAsMatrix" << "\n";
	MagLog::dev() << "GribRegularInterpretor::interpretAsMatrix" << "\n";

	Timer timer("gribapi", " read grib");
	*matrix = new Matrix();
	size_t nb;
	grib_get_size(grib.id(), "values", &nb);
	bool interpolate = grib.interpolate();
	MagLog::dev() << "numberOfFieldValues[" << nb << "]" << "\n";
	double missing = std::numeric_limits<double>::max();
	grib.setDouble("missingValue", missing);

	(*matrix)->missing(missing);
	(*matrix)->akimaEnabled();

	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double south = grib.getDouble("latitudeOfLastGridPointInDegrees");
	double east = grib.getDouble("longitudeOfLastGridPointInDegrees");
	double plp = grib.getDouble("PLPresent");
	long res = grib.getLong("numberOfParallelsBetweenAPoleAndTheEquator");

	longitudesSanityCheck(west, east);
	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << ", " << east
			<< ", " << south << "]" << "\n";
	MagLog::dev() << "PLPresent---> " << plp << "\n";
	MagLog::dev() << "Res---> " << res << "\n";

	double pl[2 * res];
	size_t aux = 2 * res;
	grib_get_double_array(grib.id(), "pl", pl, &aux);
    int nblon = 0;
    for ( int i = 0; i < aux; i++)
        if ( pl[i] > nblon) nblon = pl[i];

	double array[2 * res];
	grib_get_gaussian_latitudes(res, array);

	MagLog::dev() << "Resolution ---> " << nblon << "???" << 4 * res << "\n";

	// We have to determine if the field is global!
	if (north - south > 175.) {
		east = west + 360.;
	}

	// compute the number of points we'll be adding to the matrix so that we can
	// allocate them in one go, rather than allowing the STL to re-allocate
	// when we reach the capacity
	(*matrix)->reserve(aux * nblon);

	double *data = new double[nb];

	size_t aux2 = size_t(nb);

	double width = east - west;
	double step = (width) / (nblon);

	grib_get_double_array(grib.id(), "values", data, &aux2);

	int d = 0;
	for (size_t i = 0; i < aux; i++) {
		vector<double> p;
		for (int ii = 0; ii < pl[i]; ii++) {
			p.push_back(data[d]);
			d++;
		}

		double lon = west;
		unsigned int p1 = 0;
		unsigned int p2 = 1;
		double lon1 = west;
		double lon2 = lon1 + (width / (p.size()));

		for (int x = 0; x < nblon; x++) {

			if (lon >= lon2) {
				p1++;
				p2++;
				lon1 = lon2;
				lon2 += (width) / (p.size());
			}
			double d1 = (lon2 - lon) / (lon2 - lon1);
			double d2 = 1 - d1;
			double val;

			assert(p1 < p.size());
			if (p2 == p.size()) {
				(*matrix)->push_back(p[p1]);
			} else {
				if (interpolate) {
					if (p[p1] == missing || p[p2] == missing)
						val = missing;
					else
						val = (p[p1] * d1) + (p[p2] * d2);
				} else {
					val = (d2 < 0.5) ? p[p1] : p[p2];

				}
				(*matrix)->push_back(val);
			}
			lon += step;
		}

	}

	delete[] data;

	for (int x = 0; x < nblon; x++) {
		(*matrix)->columnsAxis().push_back(west + (x * step));
	}


	for (int i = 0; i < 2 * res; i++) {

		(*matrix)->rowsAxis().push_back(array[i]);
	}
	(*matrix)->setMapsAxis();
}


void GribReducedLatLonInterpretor::print(ostream& out) const {
	out << "GribReducedLatLonInterpretor[";
	out << "]";
}

void GribReducedLatLonInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {

	Timer timer("gribapi", " read grib");
	*matrix = new Matrix();
	size_t nb;
	grib_get_size(grib.id(), "values", &nb);


	double missing = std::numeric_limits<double>::max();
	grib.setDouble("missingValue", missing);

	(*matrix)->missing(missing);
	(*matrix)->akimaEnabled();

	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double south = grib.getDouble("latitudeOfLastGridPointInDegrees");
	double east = grib.getDouble("longitudeOfLastGridPointInDegrees");

	longitudesSanityCheck(west, east);
	size_t res = grib.getLong("Nj");

	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << ", " << east
			<< ", " << south << "]" << "\n";
	MagLog::dev() << "Res---> " << res << "\n";

	double pl[res];

	long nblat = grib.getLong("numberOfPointsAlongAMeridian");
	int scanning = grib.getLong("jScansPositively") ? 1 : -1;
	double lat = scanning * grib.getDouble("jDirectionIncrementInDegrees");

	grib_get_double_array(grib.id(), "pl", pl, &res);

	double *data = new double[nb];

	size_t aux2 = size_t(nb);

	int nblon = 0;
	for ( int i = 0; i < res; i++)
		if (nblon < pl[i] )
			nblon = pl[i];

	double width = east - west;
	double step = width / nblon;

	// We have to determine if the field is global! 
	// We have to determine if the field is global!
	bool global = east - west > 360 - 5 * step;

	if (global) {
		east = west + 360;
		width = east - west;
		step = width / nblon;
	}

	grib_get_double_array(grib.id(), "values", data, &aux2);
	int d = 0;


	for (size_t i = 0; i < res; i++) {



		if (pl[i] == 0) {
			// add missing data 
			for (int x = 0; x < nblon; x++)
				(*matrix)->push_back(missing);
		}
		else {
			unsigned int p1 = 0;
			vector<double> p;
			vector<double> lons;

			double datastep = width / pl[i];
			for (int ii = 0; ii < pl[i]; ii++) {
				p.push_back(data[d]);
				lons.push_back( west + (ii*datastep) );


				d++;
			}
			assert( p.size() ==  pl[i]);


			vector<double>::iterator val = p.begin();
			vector<double>::iterator lval = lons.begin();
			vector<double>::iterator nval = p.begin();
			nval++;
			vector<double>::iterator nlval = lons.begin();
			nlval++;

			for (int x = 0; x < nblon; x++) {

				double lon = west + (x*step);

				if ( lon > *lval ) {

					lval++;
					val++;


					if ( lval == lons.end() ) {
						val = p.begin();
						lval = lons.begin();
					}
					nval++;
					nlval++;
					if ( nlval == lons.end() ) {
						nval = p.begin();
						nlval = lons.begin();
					}


				}




				if ( *val == missing ||  *nval == missing)
					(*matrix)->push_back(*val);
				else {


					double d1 = ( *nlval - lon) / (datastep);
					if ( d1 <  0 ) d1 = 1;
					double d2 = 1 - d1;
					(*matrix)->push_back( ( d1 * (*val)) + (d2 * (*nval)));
				}




			}
		}
	}

	delete[] data;


	for (int x = 0; x < nblon; x++) {
		(*matrix)->columnsAxis().push_back(west + (x * step));
	}

	double y = north;
	for (long i = 0; i < nblat; i++) {
		(*matrix)->rowsAxis().push_back(y);
		y += lat;
	}

	(*matrix)->setMapsAxis();
}

/*
 * Imported from Metview MvGrid...
 */

void GribRotatedInterpretor::print(ostream& out) const {
	out << "GribRotatedInterpretor[]";
}

UserPoint GribLambertAzimutalInterpretor::unrotate(double lat,
		double lon) const {

	return UserPoint(lon, lat);
}

pair<double, double> GribRotatedInterpretor::unrotate(double lat_y,
		double lon_x) const {
	const double cToRadians = M_PI / 180.0;
	double ZRADI = 1. / cToRadians;
	double ZSYCEN = sin(cToRadians * (southPoleLat_ + 90.));
	double ZCYCEN = cos(cToRadians * (southPoleLat_ + 90.));
	double ZSXROT = sin(cToRadians * lon_x);
	double ZCXROT = cos(cToRadians * lon_x);
	double ZSYROT = sin(cToRadians * lat_y);
	double ZCYROT = cos(cToRadians * lat_y);
	double ZSYREG = ZCYCEN * ZSYROT + ZSYCEN * ZCYROT * ZCXROT;
	ZSYREG = MAX(MIN(ZSYREG, +1.0), -1.0);
	double PYREG = asin(ZSYREG) * ZRADI;
	double ZCYREG = cos(PYREG * cToRadians);
	double ZCXMXC = (ZCYCEN * ZCYROT * ZCXROT - ZSYCEN * ZSYROT) / ZCYREG;
	ZCXMXC = MAX(MIN(ZCXMXC, +1.0), -1.0);
	double ZSXMXC = ZCYROT * ZSXROT / ZCYREG;
	double ZXMXC = acos(ZCXMXC) * ZRADI;
	if (ZSXMXC < 0.0)
		ZXMXC = -ZXMXC;
	double PXREG = ZXMXC + southPoleLon_;
	return std::make_pair(PYREG, PXREG);
}

PaperPoint GribRotatedInterpretor::reference(const GribDecoder& grib, const Transformation& transformation) {
	double lon = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double lat = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	pair<double, double> xy = unrotate(lat, lon);
	UserPoint point(xy.second, xy.first);
	if (transformation.in(point))
			return transformation(point);


	int err;
	grib_iterator* iter = grib_iterator_new(grib.handle(), 0, &err);

	double value;
	/* Loop on all the lat/lon/values. */
	while (grib_iterator_next(iter, &lat, &lon, &value)) {
		pair<double, double> xy = unrotate(lat, lon);
		point = UserPoint(xy.second, xy.first);
		if (transformation.in(point)) {
			grib_iterator_delete(iter);
			return transformation(point);
		}
	}

	/* At the end the iterator is deleted to free memory. */
	grib_iterator_delete(iter);
	return PaperPoint(transformation.getMinPCX(), transformation.getMinPCY());
}
pair<double, double> GribRotatedInterpretor::rotate(double lat_y,
		double lon_x) const {
	const double cToRadians = M_PI / 180.0;
	double ZRADI = 1. / cToRadians;
	double ZSYCEN = sin(cToRadians * (southPoleLat_ + 90.));
	double ZCYCEN = cos(cToRadians * (southPoleLat_ + 90.));

	double ZXMXC = cToRadians * (lon_x - southPoleLon_);
	double ZSXMXC = sin(ZXMXC);
	double ZCXMXC = cos(ZXMXC);
	double ZSYREG = sin(cToRadians * lat_y);
	double ZCYREG = cos(cToRadians * lat_y);
	double ZSYROT = ZCYCEN * ZSYREG - ZSYCEN * ZCYREG * ZCXMXC;
	ZSYROT = MAX(MIN(ZSYROT, +1.0), -1.0);

	double PYROT = asin(ZSYROT) * ZRADI;

	double ZCYROT = cos(PYROT * cToRadians);
	double ZCXROT = (ZCYCEN * ZCYREG * ZCXMXC + ZSYCEN * ZSYREG) / ZCYROT;
	ZCXROT = MAX(MIN(ZCXROT, +1.0), -1.0);
	double ZSXROT = ZCYREG * ZSXMXC / ZCYROT;

	double PXROT = acos(ZCXROT) * ZRADI;

	if (ZSXROT < 0.0)
		PXROT = -PXROT;

	return std::make_pair(PYROT, PXROT);
}

void GribLambertAzimutalInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {
	long im = grib.getLong("numberOfPointsAlongXAxis");
	long jm = grib.getLong("numberOfPointsAlongYAxis");

	RotatedMatrix *rotated = new RotatedMatrix(jm, im);
	*matrix = rotated;

	size_t nb;
	grib_get_size(grib.id(), "values", &nb);

	MagLog::dev() << "numberOfFieldValues[" << nb << "]" << "\n";

	double missing = -std::numeric_limits<double>::max();

	missing = grib.getDouble("missingValue");
	rotated->missing(missing);

	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");

	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << "]" << "\n";

	try {
		Timer time("Grib", "lambert");

		MagLog::debug() << "Version" << grib_get_api_version() << endl;

		vector<double>& data = rotated->values();
		vector<double>& latm = rotated->rowsArray();
		vector<double>& lonm = rotated->columnsArray();

		size_t aux = size_t(nb);

		grib_get_double_array(grib.id(), "latitudes", &(latm.front()), &aux);
		grib_get_double_array(grib.id(), "values", &(data.front()), &aux);
		grib_get_double_array(grib.id(), "longitudes", &(lonm.front()), &aux);
		for (int i = 0; i < nb; i++) {

			if (lonm[i] > 180.)
				lonm[i] -= 360.;

		}
		/*
		 vector<double> left;
		 vector<double> right;
		 for (int l = 0; l < jm; l++ ) {
		 left.push_back(lonm[l*im]);
		 right.push_back(lonm[(im-1)+(l*im)]);


		 }
		 minlon = *min_element(left.begin(), left.end());
		 maxlon = *max_element(right.begin(), right.end());

		 MagLog::debug() << "lat [" << minlat << ", " << maxlat << "]" << std::endl;
		 MagLog::debug()	<< "lon [" << minlon << ", " << maxlon << "]" << std::endl;





		 vector<double>& lon = (*matrix)->columnsAxis();
		 for (vector<double>::iterator l = lon.begin(); l != lon.end(); ++l )
		 if ( *l > 180) *l  -=360;

		 vector<double>& lat = (*matrix)->rowsAxis();

		 // for the lon we take the fisrt line :
		 double inci = (maxlon - minlon)/((im) -1);
		 double incj = (maxlat - minlat)/((jm) -1);
		 for (int i = 0; i < im; i++)
		 lon.push_back(minlon + (i*inci));
		 // for the lon we take the fisrt column :
		 for (int i = 0; i < jm; i++)
		 lat.push_back(minlat + (i*incj));

		 typedef map<double, map<double, pair<int, int> > > Helper;

		 //typedef map<double, double> Helper;
		 Helper helper;
		 int row = 0;
		 for (vector<double>::iterator y = lat.begin(); y != lat.end(); ++y) {

		 helper.insert(make_pair(*y, map<double, pair<int, int>  >()));

		 Helper::iterator h = helper.find(*y);

		 int column = 0;
		 for (vector<double>::iterator x = lon.begin(); x != lon.end(); ++x) {
		 h->second.insert(make_pair(*x, std::make_pair(row, column)));

		 (*matrix)->push_back(missing);
		 column++;

		 }
		 row++;
		 }



		 int r = 0;
		 int c = 0;

		 double lat11, lat12, lat21, lat22;
		 double lon11, lon12, lon21, lon22;
		 double val11, val12, val21, val22;

		 for (int r = 0; r < jm -1; r++) {
		 for (int c = 0;  c < im -1; c++) {

		 lat11 = latm[c + (im*r)];

		 lat12 = latm[(c+1) + (im*r)];
		 minlat = std::min(lat11, lat12);
		 maxlat = std::max(lat11, lat12);
		 lat21 = latm[c + (im* (r+1))];
		 minlat = std::min(minlat, lat21);
		 maxlat = std::max(maxlat, lat21);
		 lat22 = latm[(c+1) + (im* (r+1))];
		 minlat = std::min(minlat, lat22);
		 maxlat = std::max(maxlat, lat22);

		 lon11 = lonm[c + (im*r)];
		 lon12 = lonm[(c+1) + (im*r)];
		 if ( lon12 < lon11 )
		 lon12 +=360.;
		 minlon = std::min(lon11, lon12);
		 maxlon = std::max(lon11, lon12);
		 lon21 = lonm[c + (im* (r+1))];
		 minlon = std::min(minlon, lon21);
		 maxlon = std::max(maxl  inline double column(int, int column) const {
		 return regular_longitudes_[column];
		 }
		 inline double row(int row, int) const {
		 return regular_latitudes_[row];
		 }on, lon21);
		 lon22 = lonm[(c+1) + (im* (r+1))];
		 if ( lon22 < lon21 )
		 lon22 +=360.;
		 minlon = std::min(minlon, lon22);
		 maxlon = std::max(maxlon, lon22);

		 val11 = data[c + (im*r)];
		 val12 = data[(c+1) + (im*r)];
		 val21 = data[c + (im* (r+1))];
		 val22 = data[(c+1) + (im* (r+1))];



		 // find the points from the helper!
		 Helper::iterator low,up;
		 low = helper.lower_bound(minlat);
		 up = helper.lower_bound(maxlat);
		 if ( low == helper.end() || up == helper.end() )
		 break;
		 for (Helper::iterator it = low; it != up; ++it) {
		 if (it == helper.end()) break;
		 map<double, pair<int, int> >&  lons = it->second;
		 map<double, pair<int, int> >::iterator llow = lons.lower_bound(minlon);
		 map<double, pair<int, int> >::iterator lup = lons.lower_bound(maxlon);
		 if ( llow == lons.end() || lup == lons.end() )
		 break;;
		 for (map<double, pair<int, int> >::iterator lit = llow; lit != lup; ++lit) {

		 double lat = it->first;
		 double lon = lit->first;
		 std::pair<int, int> index = lit->second;

		 // we interpolate at the point using the 4 points found!
		 double val = missing;
		 map<double, double> values;
		 vector<double> distances;

		 distances.push_back((lon12 - lon)*(lon12 - lon) + (lat12 -lat) *(lat12 -lat));
		 values[distances.back()] = val12;
		 distances.push_back((lon11 - lon)*(lon11 - lon) + (lat11 -lat) *(lat11 -lat));
		 values[distances.back()] = val11;
		 distances.push_back((lon21 - lon)*(lon21 - lon) + (lat21 -lat) *(lat21 -lat));
		 values[distances.back()] = val21;
		 distances.push_back((lon22 - lon)*(lon22 - lon) + (lat22 -lat) *(lat22 -lat));
		 values[distances.back()] = val22;
		 if ( (**matrix)[index.second +( index.first*im)] == missing )
		 (**matrix)[index.second +( index.first*im)] = values[*std::min_element(distances.begin(), distances.end())];

		 //we compute the distance ... we take the vlaue of the nearest non_missing point!


		 }


		 }


		 }


		 (*matrix)->setMapsAxis();
		 (*matrix)->missing(missing);
		 }





		 MagLog::dev() << **matrix << "\n";


		 */
	} catch (MagicsException& e) {
		MagLog::error() << e << "\n";
	}

}

void GribLambertAzimutalInterpretor::print(ostream& out) const {
	out << "GribLambertAzimutalInterpretor[]";
}

void GribRotatedInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {

	southPoleLat_ = grib.getDouble("latitudeOfSouthernPoleInDegrees");
	southPoleLon_ = grib.getDouble("longitudeOfSouthernPoleInDegrees");
	angle_ = grib.getDouble("angleOfRotationInDegrees") * 180.0 / M_PI;
	uvRelativeToGrid_ = grib.getLong("uvRelativeToGrid");
	if (original_) {
		long nblon = grib.getLong("numberOfPointsAlongAParallel");
		long nblat = grib.getLong("numberOfPointsAlongAMeridian");

		*matrix = new RotatedMatrix(nblat, nblon);
		size_t nb;
		grib_get_size(grib.id(), "values", &nb);

		MagLog::dev() << "numberOfFieldValues[" << nb << "]" << "\n";
		double missing = -std::numeric_limits<double>::max();
		grib.setDouble("missingValue", missing);

		(*matrix)->missing(missing);

		double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
		double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");
		double south = grib.getDouble("latitudeOfLastGridPointInDegrees");
		;
		double east = grib.getDouble("longitudeOfLastGridPointInDegrees");
		;
		longitudesSanityCheck(west, east);

		double lon = (east - west) / (nblon - 1);
		double lat = (south - north) / (nblat - 1);

		vector<double>& rows =
				static_cast<RotatedMatrix*>(*matrix)->rowsArray();
		vector<double>& columns =
				static_cast<RotatedMatrix*>(*matrix)->columnsArray();
		vector<double>& values = static_cast<RotatedMatrix*>(*matrix)->values();
		MagLog::dev() << "calcul -->" << lon << " (from->" << west << " to-->"
				<< west + (nblon - 1) * lon << ")" << endl;

		double y = north;
		for (int i = 0; i < nblat; i++) {
			double x = west;
			for (int j = 0; j < nblon; j++) {
				if (i == 0)
					(*matrix)->columnsAxis().push_back(x);
				std::pair<double, double> point = unrotate(y, x);
				columns.push_back(point.second);
				rows.push_back(point.first);
				x = west + (j + 1) * lon;

			}

			y = north + (i + 1) * lat;

		}

		try {
			(*matrix)->resize(nb);
			size_t aux = size_t(nb);
			grib_get_double_array(grib.id(), "values", &values.front(), &aux);
		} catch (...) {
			throw MagicsException("Not enough memory");
		}

		return;
	}

	long nblon = grib.getLong("numberOfPointsAlongAParallel");
	long nblat = grib.getLong("numberOfPointsAlongAMeridian");
	*matrix = new Matrix(nblat, nblon);

	Matrix* helper = new Matrix(nblat, nblon); // settup as the equivalent regular matrix! 

	size_t nb;
	grib_get_size(grib.id(), "values", &nb);

	MagLog::dev() << "numberOfFieldValues[" << nb << "]" << "\n";
	double missing = -std::numeric_limits<double>::max();
	grib.setDouble("missingValue", missing);
	helper->missing(missing);
	(*matrix)->missing(missing);

	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");
	double south = grib.getDouble("latitudeOfLastGridPointInDegrees");
	;
	double east = grib.getDouble("longitudeOfLastGridPointInDegrees");
	;
	longitudesSanityCheck(west, east);

	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << ", " << east
			<< ", " << south << "]" << "\n";

	double lon = (east - west) / (nblon - 1);
	double lat = (south - north) / (nblat - 1);

	MagLog::dev() << "calcul -->" << lon << " (from->" << west << " to-->"
			<< west + (nblon - 1) * lon << ")" << endl;

	double y = north;
	for (int i = 0; i < nblat; i++) {

		helper->rowsAxis().push_back(y);
		y = north + (i + 1) * lat;

	}

	double x = west;
	for (int i = 0; i < nblon; i++) {

		helper->columnsAxis().push_back(x);
		x = west + (i + 1) * lon;

	}

	helper->setMapsAxis();

	try {
		helper->resize(nb);
		size_t aux = size_t(nb);
		grib_get_double_array(grib.id(), "values", &helper->front(), &aux);
	} catch (...) {
		throw MagicsException("Not enough memory");
	}

	lon = west;
	lat = north;
	double steplon = (east - west) / (nblon - 1);
	double steplat = (south - north) / (nblat - 1);

	// Fisrt try to find the bounding box 
	vector<double> rows;
	rows.reserve(nb);
	vector<double> columns;
	columns.reserve(nb);
	vector<double> values;
	values.reserve(nb);

	for (int j = 0; j < nblat; j++) {
		lon = west;

		for (int i = 0; i < nblon; i++) {
			std::pair<double, double> point = unrotate(lat, lon);

			rows.push_back(point.first);
			columns.push_back(point.second);

			lon += steplon;
		}
		lat += steplat;
	}

	double minx = *std::min_element(columns.begin(), columns.end());
	double maxx = *std::max_element(columns.begin(), columns.end());
	double miny = *std::min_element(rows.begin(), rows.end());
	double maxy = *std::max_element(rows.begin(), rows.end());

	double stepx = (maxx - minx) / (nblon - 1);
	double stepy = (maxy - miny) / (nblat - 1);
	x = minx;
	y = miny;
	// Create the Axis for Regular Matrix..
	for (int i = 0; i < nblon; i++) {
		(*matrix)->columnsAxis().push_back(x);
		x += stepx;
	}
	for (int j = 0; j < nblat; j++) {
		(*matrix)->rowsAxis().push_back(y);
		y += stepy;
	}

	miny = std::min(north, south);
	maxy = std::max(north, south);
	minx = std::min(west, east);
	maxx = std::max(west, east);

	(*matrix)->setMapsAxis();
	for (int j = 0; j < nblat; j++) {
		for (int i = 0; i < nblon; i++) {

			pair<double, double> point = rotate((*matrix)->row(j, i),
					(*matrix)->column(j, i));
			if (point.first > miny && point.first < maxy && point.second > minx
					&& point.second < maxx) {
				(*matrix)->push_back(
						helper->interpolate(point.first, point.second));

			} else {
				(*matrix)->push_back(missing);

			}
		}
	}
}

void GribRotatedInterpretor::interpret2D(double& lat, double& lon, double& uc,
		double& vc) const {
	if (!uvRelativeToGrid_)
		return;
	double speed = sqrt((uc * uc) + (vc * vc));
	double angle = atan2(vc, uc);

	std::pair<double, double> pt = rotate(lat, lon);
	std::pair<double, double> pv = unrotate(pt.first, pt.second + 1);

	double rangle = atan2(pv.first - lat, pv.second - lon) + angle;
	//components.second *= f;
	// we the angle and the spped we compute u/v...
	uc = speed * cos(rangle);
	vc = speed * sin(rangle);

}

void GribRotatedInterpretor::raw(const GribDecoder& grib,
		const Transformation& transformation, const string& key,
		map<double, map<double, CustomisedPoint*> >& points) const {
	double factor, offset;
	scaling(grib, factor, offset);
	int err;
	southPoleLat_ = grib.getDouble("latitudeOfSouthernPoleInDegrees");
	southPoleLon_ = grib.getDouble("longitudeOfSouthernPoleInDegrees");
	angle_ = grib.getDouble("angleOfRotationInDegrees") * 180.0 / M_PI;
	;
	grib_iterator* iter = grib_iterator_new(grib.handle(), 0, &err);
	double missing = grib.getDouble("missingValue");

	double lat, lon, value;
	/* Loop on all the lat/lon/values. */
	while (grib_iterator_next(iter, &lat, &lon, &value)) {

		pair<double, double> coords = unrotate(lat, lon);
		lat = coords.first;
		lon = coords.second;
		if (value != missing) {
			/*
			 std::stack<UserPoint>   duplicates;
			 UserPoint geo(lon, lat);
			 value = (value*factor)+offset;
			 transformation.populate(lon, lat, value, points);
			 */

		}

	}

	/* At the end the iterator is deleted to free memory. */
	grib_iterator_delete(iter);

}

void GribLambertInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {
	long im = grib.getLong("numberOfPointsAlongXAxis");
	long jm = grib.getLong("numberOfPointsAlongYAxis");

	*matrix = new Matrix(im, jm);

	size_t nb;
	grib_get_size(grib.id(), "values", &nb);

	MagLog::dev() << "numberOfFieldValues[" << nb << "]" << "\n";
	double missing = -std::numeric_limits<double>::max();
	missing = grib.getDouble("missingValue");

	double north = grib.getDouble("latitudeOfFirstGridPointInDegrees");
	double west = grib.getDouble("longitudeOfFirstGridPointInDegrees");

	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << "]" << "\n";

	try {
		Timer time("Grib", "lambert");

		MagLog::debug() << "Version" << grib_get_api_version() << endl;

		vector<double> latm;
		vector<double> lonm;
		vector<double> data;
		latm.reserve(nb);
		lonm.reserve(nb);
		data.reserve(nb);

		size_t aux = size_t(nb);

		grib_get_double_array(grib.id(), "latitudes", &(latm.front()), &aux);

		double minlat = *min_element(latm.begin(), latm.end());
		double maxlat = *max_element(latm.begin(), latm.end());
		;
		grib_get_double_array(grib.id(), "longitudes", &(lonm.front()), &aux);

		double minlon = *min_element(lonm.begin(), lonm.end());
		double maxlon = *max_element(lonm.begin(), lonm.end());
		;

		// This test needs to be improved ...

		if (minlon > 50.)
			minlon -= 360;
		if (maxlon > 50.)
			maxlon -= 360;

		grib_get_double_array(grib.id(), "values", &(data.front()), &aux);
		double min = *min_element(data.begin(), data.end());
		double max = *max_element(data.begin(), data.end());
		;

		for (int i = 0; i < nb; i++) {
			if (lonm[i] > 50.)
				lonm[i] = lonm[i] - 360;

			if (minlat > latm[i])
				minlat = latm[i];
			if (maxlat < latm[i])
				maxlat = latm[i];
			if (minlon > lonm[i])
				minlon = lonm[i];
			if (maxlon < lonm[i])
				maxlon = lonm[i];
		}

		MagLog::debug() << "lat [" << minlat << ", " << maxlat << "]"
				<< std::endl;
		MagLog::debug() << "lon [" << minlon << ", " << maxlon << "]"
				<< std::endl;

		vector<double>& lon = (*matrix)->columnsAxis();
		vector<double>& lat = (*matrix)->rowsAxis();

		// for the lon we take the fisrt line :
		double inci = (maxlon - minlon) / ((im) - 1);
		double incj = (maxlat - minlat) / ((jm) - 1);
		for (int i = 0; i < im; i++)
			lon.push_back(minlon + (i * inci));
		// for the lon we take the fisrt column :
		for (int i = 0; i < jm; i++)
			lat.push_back(minlat + (i * incj));

		typedef map<double, map<double, pair<int, int> > > Helper;

		//typedef map<double, double> Helper;
		Helper helper;
		int row = 0;
		for (vector<double>::iterator y = lat.begin(); y != lat.end(); ++y) {

			helper.insert(make_pair(*y, map<double, pair<int, int> >()));

			Helper::iterator h = helper.find(*y);

			int column = 0;
			for (vector<double>::iterator x = lon.begin(); x != lon.end();
					++x) {
				h->second.insert(make_pair(*x, std::make_pair(row, column)));

				(*matrix)->push_back(missing);
				column++;

			}
			row++;
		}

		int r = 0;
		int c = 0;

		double lat11, lat12, lat21, lat22;
		double lon11, lon12, lon21, lon22;
		double val11, val12, val21, val22;

		for (int r = 0; r < jm - 1; r++) {
			for (int c = 0; c < im - 1; c++) {

				lat11 = latm[c + (im * r)];

				lat12 = latm[(c + 1) + (im * r)];
				minlat = std::min(lat11, lat12);
				maxlat = std::max(lat11, lat12);
				lat21 = latm[c + (im * (r + 1))];
				minlat = std::min(minlat, lat21);
				maxlat = std::max(maxlat, lat21);
				lat22 = latm[(c + 1) + (im * (r + 1))];
				minlat = std::min(minlat, lat22);
				maxlat = std::max(maxlat, lat22);

				lon11 = lonm[c + (im * r)];
				lon12 = lonm[(c + 1) + (im * r)];
				if (lon12 < lon11)
					lon12 += 360.;
				minlon = std::min(lon11, lon12);
				maxlon = std::max(lon11, lon12);
				lon21 = lonm[c + (im * (r + 1))];
				minlon = std::min(minlon, lon21);
				maxlon = std::max(maxlon, lon21);
				lon22 = lonm[(c + 1) + (im * (r + 1))];
				if (lon22 < lon21)
					lon22 += 360.;
				minlon = std::min(minlon, lon22);
				maxlon = std::max(maxlon, lon22);

				val11 = data[c + (im * r)];
				val12 = data[(c + 1) + (im * r)];
				val21 = data[c + (im * (r + 1))];
				val22 = data[(c + 1) + (im * (r + 1))];

				// find the points from the helper!
				Helper::iterator low, up;
				low = helper.lower_bound(minlat);
				up = helper.lower_bound(maxlat);
				if (low == helper.end() || up == helper.end())
					break;
				for (Helper::iterator it = low; it != up; ++it) {
					if (it == helper.end())
						break;
					map<double, pair<int, int> >& lons = it->second;
					map<double, pair<int, int> >::iterator llow =
							lons.lower_bound(minlon);
					map<double, pair<int, int> >::iterator lup =
							lons.lower_bound(maxlon);
					if (llow == lons.end() || lup == lons.end())
						break;;
					for (map<double, pair<int, int> >::iterator lit = llow;
							lit != lup; ++lit) {

						double lat = it->first;
						double lon = lit->first;
						std::pair<int, int> index = lit->second;

						// we interpolate at the point using the 4 points found!
						double val = missing;
						if (val11 != missing && val12 != missing
								&& val21 != missing && val22 != missing) {
							double val1 = ((lon12 - lon) / (lon12 - lon11))
									* val11
									+ ((lon - lon11) / (lon12 - lon11)) * val12;

							double val2 = ((lon22 - lon) / (lon22 - lon21))
									* val21
									+ ((lon - lon21) / (lon22 - lon21)) * val22;
							if (isnan(val1)) {
								if (isnan(val2)) {
									val = missing;
								} else
									val = ((lat - lat11) / (lat22 - lat11))
											* val2;
							} else {
								if (isnan(val2)) {
									val = ((lat22 - lat) / (lat22 - lat11))
											* val1;
								} else {
									val = ((lat22 - lat) / (lat22 - lat11))
											* val1
											+ ((lat - lat11) / (lat22 - lat11))
													* val2;
								}
							}

							if (isnan(val) || isinf(val) || isinf(-val)) {
								val = missing;
							}
						}
						if (isnan(val))
							val = missing;
						if ((**matrix)[index.second + (index.first * im)]
								== missing) {
							(**matrix)[index.second + (index.first * im)] = val;

						}

					}

				}

			}

			(*matrix)->setMapsAxis();
			(*matrix)->missing(missing);
		}

		MagLog::dev() << **matrix << "\n";

	}

	catch (MagicsException& e) {
		MagLog::error() << e << "\n";
	}

}
void GribLambertInterpretor::print(ostream& out) const {
	out << "GribLambertInterpretor[";
	out << "]";
}



PaperPoint GribLambertAzimutalInterpretor::reference(const GribDecoder&, const Transformation&)
{
	assert(false);

}

void GribPolarStereoInterpretor::print(ostream& out) const {
	out << "GribLambertInterpretor[";
	out << "]";
}
void GribPolarStereoInterpretor::interpretAsMatrix(const GribDecoder& grib,
		Matrix** matrix) const {
	long im = 3600;
	long jm = 1800;

	*matrix = new Matrix(im, jm);
	double steplon= 0.1;
	double lon0 = 0 - (steplon/2);
	double steplat= 0.1;
	double lat0 = -90 - (steplat/2);

	double missing =  grib.getDouble("missingValue");
	for (int i = 0; i < im; i++) {
		double x = 0 + (i*steplon);
		(*matrix)->columnsAxis().push_back(x);

	}

	for (int i = 0; i < jm; i++) {
		double y = -90 + (i*steplat);
		(*matrix)->rowsAxis().push_back(y);


	}
	vector<double> distance(im*jm, 999999);


	for (int i = 0; i < im; i++) {
		for (int j = 0; j < jm; j++)
		(**matrix)[i + (j * im)] = missing;
	}
	(*matrix)->missing(missing);
	(*matrix)->setMapsAxis();

	interpolate(grib, **matrix);


	MagLog::dev() << **matrix << "\n";




}

void GribInterpretor::interpolate(const GribDecoder& grib, Matrix& matrix) const
{
	int err;

	grib_iterator* iter = grib_iterator_new(grib.handle(), 0, &err);
	if ( err ) {
		MagLog::warning() << "Grib Iterator not available : Chech Grib Api Version " << grib_get_api_version() << endl;
		return;
	}
	double xres = matrix.XResolution();
	double yres = matrix.YResolution();

	double xbase = matrix.left() - (xres/2);
	double ybase = matrix.bottom() - (yres/2);
	double columns = matrix.columns();
	double rows = matrix.rows();

	vector<double> distance(columns*rows, 999999);

	double lat, lon, value;
	/* Loop on all the lat/lon/values. */
	while (grib_iterator_next(iter, &lat, &lon, &value)) {
		int ix =  (lon - xbase )/xres;
		double x = (ix * xres) + xbase;
		int iy =  (lat - ybase )/yres;
		double y = (iy * yres) + ybase;
		double dist = ((lon - x)*(lon - x)) + ((lat - y)*(lat - y));
		if ( dist < distance[ix + (iy * columns)] ) {
			matrix[ix + (iy * columns)] = value;
			distance[ix + (iy * columns)]= dist;
		}
	}
	/* At the end the iterator is deleted to free memory. */
	grib_iterator_delete(iter);







}

