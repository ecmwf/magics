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

     size_t nb=0;
     grib_get_size(grib.id(), "values", &nb);
     raster.reserve(nb);

     // If value is temperature in degrees K then add 145 to pixel value
     double offset = (grib.getLong("functionCode") == 1) ? 145. : 0.;
     
    grib_get_double_array(grib.id(),"values",&raster.front(),&nb);
    
     if (offset) { 
    	 for (unsigned int i = 0; i < nb; i++) {
    		 	raster[i] +=offset;
    	       
    	 }
     }
     

}

static SimpleObjectMaker<GribSatelliteInterpretor, GribInterpretor> gribsatellite("90");
