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

/*! \file GribSatelliteInterpretor.cc
    \brief Implementation of the Template class GribSatelliteInterpretor.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon 18-Apr-2005

    Changes:
*/


#include "CustomisedPoint.h"
#include "GribSatelliteInterpretor.h"
#include "GribDecoder.h"
#include "RasterData.h"
#include "TeProjection.h"
#include "TeDataTypes.h"
#include "TeRasterParams.h"
#include "TeDecoderMemory.h"
#include "TeRasterRemap.h"
#include "GeoRectangularProjection.h"

using namespace magics;

GribSatelliteInterpretor::GribSatelliteInterpretor() 
{
}


GribSatelliteInterpretor::~GribSatelliteInterpretor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void GribSatelliteInterpretor::print(ostream& out)  const
{
     out << "GribSatelliteInterpretor[";
     out << "]";
}
void GribSatelliteInterpretor::interpretAsMatrix(const GribDecoder& grib, Matrix** matrix) const
{
	Timer timer("gribapi", " reproject satellite");
	MagLog::dev() << "GribRegularInterpretor::interpretAsMatrix" << "\n";

	double altitude = grib.getDouble("NrInRadiusOfEarth");
	if ( !altitude ) altitude =  6610839.;
	altitude *= 0.000001;
	long   nx   = grib.getLong("numberOfPointsAlongXAxis");
	long   ny   = grib.getLong("numberOfPointsAlongYAxis");
	double offx = grib.getDouble("xCoordinateOfOriginOfSectorImage");
	double offy = grib.getDouble("yCoordinateOfOriginOfSectorImage");
	double prj  = 2*asin(1/altitude)/grib.getDouble("dx");
	double pri  = 2*asin(1/altitude)/grib.getDouble("dy");
	double pjs  = grib.getDouble("XpInGridLengths");
	double pis  = grib.getDouble("YpInGridLengths");
	double lao  = grib.getDouble("latitudeOfSubSatellitePointInDegrees") *TeCDR;
	double lono = grib.getDouble("longitudeOfSubSatellitePointInDegrees")*TeCDR;
	double prs  = altitude * TeEARTHRADIUS;
	double scn  = 0;
	double yaw  = grib.getDouble("orientationOfTheGrid");

	yaw = RAD(yaw/1000);
	if (yaw < 0.) yaw += PI;
	else          yaw -= PI;

	double resx = (double)(abs(( atan( tan(pri) * (altitude-1.) ) * TeEARTHRADIUS )));
	double resy = (double)(abs(( atan( tan(prj) * (altitude-1.) ) * TeEARTHRADIUS )));

	double west  = offx * resx;
	double east  = (offx + nx - 1) * resx;
	double north = -(offy * resy);
	double south = -(offy + ny - 1) * resy;

	TeSatelliteProjection* projection = new TeSatelliteProjection(TeDatum(), offx, offy, pri, prj, pis, pjs, lao, lono, prs, scn, yaw);

	TeRasterParams in;
	in.ncols_  = nx;
	in.nlines_ = ny;
	in.resx_   = resx;
	in.resy_   = resy;
	in.projection (projection);

	in.boundingBoxResolution(west, south, east, north, resx, resy, TeBox::TeLOWERLEFT);
	in.nBands(1);
	in.setDataType(TeUNSIGNEDSHORT);

	in.setCompressionMode(TeRasterParams::TeNoCompression);
	in.decName("MEM");

		// Copy input image to the raster structure
	TeRaster rastin(in);

	bool ok = rastin.init(in);

	long hasBitmap = grib.getLong("bitmapPresent");
	double missingValue = 65535;
	if (hasBitmap)
			grib_set_double(grib.id(),"missingValue", missingValue);

		// get the array of values
	size_t nb=0;
	grib_get_size(grib.id(), "values", &nb);
	vector<double> raster(nb);
	grib_get_double_array(grib.id(),"values",&raster.front(),&nb);


		// If value is temperature in degrees K then add 145 to pixel value
		double offset = (grib.getLong("functionCode") == 1) ? 145. : 0.;

		if (offset) {
			for (unsigned int i = 0; i < nb; i++) {
				if (hasBitmap && raster[i]!=missingValue)
					raster[i] +=offset;

			}
		}
	vector<double>::iterator val = raster.begin();

	for (int i = 0; i < ny; i++)
		for (int j = 0; j < nx; j++) {
			rastin.setElement(i,j,*val, 0);
			++val;
		}




	GeoRectangularProjection latlon;
	TeRasterParams parout;
	TeProjection& projout = latlon.getProjection();
	parout.projection(&projout);
	parout.decName("MEM");

	parout.boundingBoxResolution(latlon.getMinPCX(),latlon.getMinPCY(), latlon.getMaxPCX(), latlon.getMaxPCY(),0.1,0.1,TeBox::TeLOWERLEFT );
	parout.decName("MEM");
	parout.nBands(1);
	parout.setDataType(TeUNSIGNEDSHORT);
	parout.setCompressionMode(TeRasterParams::TeNoCompression);

	// Initialise raster structure
	TeRaster rastout(parout);
	ok = rastout.init(parout);

	// Reproject input data
	TeRasterRemap reproj(&rastin,&rastout);
	ok = reproj.apply();


	long nblon = parout.ncols_;
	long nblat =  parout.nlines_;

	if ( *matrix == 0 ) *matrix = new Matrix(nblat, nblon);



	double missing = INT_MAX;
	grib.setDouble("missingValue", missing);
	(*matrix)->missing(missing);


	north = latlon.getMaxPCY();
	west  =  latlon.getMinPCX();
	south = latlon.getMinPCY();
	east  = latlon.getMaxPCX();




	MagLog::dev() << "NewAPI---> area[" << west << ", " << north << ", " << east << ", " << south << "]" << "\n";

	double lon = (east-west)/(nblon-1);
	double lat = (south-north)/(nblat-1);

	MagLog::dev() << "calcul -->" << lon << " (from->" << west << " to-->" << west + (nblon-1) *lon << ")" <<  endl;

	double x = west;
	for (int i = 1; i <= nblon; i++)
	{

		(*matrix)->columnsAxis().push_back(x);
		x  = west + (i*lon);
	}
	double y = north;
	for (int i = 1; i <= nblat; i++)
	{

		(*matrix)->rowsAxis().push_back(y);
		y  = north + (i*lat);
	}
	(*matrix)->setMapsAxis();


	int k = 0;

	for (int j=0;j < nblat;j++) {

	        for (int i=0;i < nblon;i++) {
	            double val;
	            rastout.getElement(i,j,val,0);
	            (**matrix)[k++] = val;

		      }

	      }


		(*matrix)->missing(missingValue);



}


void GribSatelliteInterpretor::interpretAsRaster(const GribDecoder& grib, RasterData& raster,const Transformation&) const
{
     MagLog::dev() << "GribSatelliteInterpretor::interpretAsRaster" << "\n";
/*
     \param datum:    planimetric datum
     \param offx:     x offset
     \param offy:     y offset
     \param Pri:      Sensor angle resolution along y axis in radians
     \param Prj:      Sensor angle resolution along x axis in radians
     \param Pis:      Y-coordinate of sub-satellite point 
     \param Pjs:      X-coordinate of sub-satellite point
     \param Pla0:     Latitude of sub-satellite point in radians
     \param Plo0:     Longitude of sub-satellite point in radians
     \param Prs:      Radius of satellite orbit in meters
     \param Pscn:     Scanning mode: 0-WE/NS, 1-SN/EW
     \param Pyaw:     Grid orientation, i.e., angle in radians between
			 the increasing y axis and the meridian of the
			 sub-satellite point along the direction of
			 increasing latitude.
*/
     double altitude = grib.getDouble("NrInRadiusOfEarth") * 0.000001;
     long   nx   = grib.getLong("numberOfPointsAlongXAxis");
     long   ny   = grib.getLong("numberOfPointsAlongYAxis");
     double offx = grib.getDouble("xCoordinateOfOriginOfSectorImage");
     double offy = grib.getDouble("yCoordinateOfOriginOfSectorImage");
     double prj  = 2*asin(1/altitude)/grib.getDouble("dx");
     double pri  = 2*asin(1/altitude)/grib.getDouble("dy");
     double pjs  = grib.getDouble("XpInGridLengths");
     double pis  = grib.getDouble("YpInGridLengths");
     double lao  = grib.getDouble("latitudeOfSubSatellitePointInDegrees") *TeCDR;
     double lono = grib.getDouble("longitudeOfSubSatellitePointInDegrees")*TeCDR;
     double prs  = altitude * TeEARTHRADIUS;
     double scn  = 0; // scanning mode later! 
     double yaw  = grib.getDouble("orientationOfTheGrid");

     yaw = RAD(yaw/1000);
     if (yaw < 0.) yaw += PI;
     else          yaw -= PI; 

     TeSatelliteProjection* projection = new TeSatelliteProjection(TeDatum(), offx, offy, pri, prj, pis, pjs, lao, lono, prs, scn, yaw);

     double resx = (double)( atan( tan(pri) * (altitude-1.) ) * TeEARTHRADIUS );
     double resy = (double)( atan( tan(prj) * (altitude-1.) ) * TeEARTHRADIUS );

     double west  = offx * resx;
     double east  = (offx + nx - 1) * resx;
     double north = -(offy * resy);
     double south = -(offy + ny - 1) * resy;

     raster.setXResolution(resx);
     raster.setYResolution(resy);
     raster.setColumns(nx);
     raster.setRows(ny);
     raster.setUpperRightCorner(east, north);
     raster.setLowerLeftCorner(west, south);
     raster.setProjection(projection);

     // set the missing value indicator?
     // note that it seems to be the case that we should ensure that this number
     // is the same as is set in Metview/ReprojectionService.cc
     long hasBitmap = grib.getLong("bitmapPresent");
     double missingValue = 65535;
     if (hasBitmap)
        grib_set_double(grib.id(),"missingValue", missingValue);

     // get the array of values
     size_t nb=0;
     grib_get_size(grib.id(), "values", &nb);
     raster.resize(nb);
     grib_get_double_array(grib.id(),"values",&raster.front(),&nb);


     // If value is temperature in degrees K then add 145 to pixel value
     double offset = (grib.getLong("functionCode") == 1) ? 145. : 0.;

     if (offset) { 
        for (unsigned int i = 0; i < nb; i++) {
            if (!hasBitmap || raster[i]!=missingValue)  // if not missing value
                raster[i] +=offset;
        }
     }

}

static SimpleObjectMaker<GribSatelliteInterpretor, GribInterpretor> gribsatellite("90");
