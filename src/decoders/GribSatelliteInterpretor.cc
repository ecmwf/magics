/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribSatelliteInterpretor.cc
    \brief Implementation of the Template class GribSatelliteInterpretor.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mon 18-Apr-2005

    Changes:
*/


#include "GribSatelliteInterpretor.h"
#include "CustomisedPoint.h"
#include "GeoRectangularProjection.h"
#include "GribDecoder.h"
#include "RasterData.h"

using namespace magics;


////////////////////////////

/* parameters used in the routines as given in Ref. [1] */
// const double  PI         =     3.14159265359;
const double SAT_HEIGHT = 42164.0;   /* distance from Earth centre to satellite     */
const double R_EQ       = 6378.169;  /* radius from Earth centre to equator         */
const double R_POL      = 6356.5838; /* radius from Earth centre to pol             */
// const double  SUB_LON    =     0.0;       /* longitude of sub-satellite point in radiant */

// const double  CFAC_NONHRV  =  -781648343;      /* scaling coefficients (see note above)  */
// const double  LFAC_NONHRV  =  -781648343;      /* scaling coefficients (see note above)  */

// const double  CFAC_HRV     =   -2344945030.;   /* scaling coefficients (see note above)  */
// const double  LFAC_HRV     =   -2344945030.;   /* scaling coefficients (see note above)  */


const long COFF_NONHRV = 1856; /* scaling coefficients (see note above)  */
const long LOFF_NONHRV = 1856; /* scaling coefficients (see note above)  */

const long COFF_HRV = 5566; /* scaling coefficients (see note above)  */
const long LOFF_HRV = 5566; /* scaling coefficients (see note above)  */


//////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include <stdlib.h>


/* this function returns the nearest integer to the value val */
/* and is used in function geocoord2pixcoord */

int nint(double val) {
    double a = 0.0; /* integral  part of val */
    double b = 0.0; /* frational part of val */

    b = modf(val, &a);

    if (b > 0.5) {
        val = ceil(val);
    }
    else {
        val = floor(val);
    }

    return (int)val;
}

/**************************************************************
 * function geocoord2pixcoord                                 *
 *                                                            *
 * PURPOSE:                                                   *
 *   return the pixel column and line of an MSG image         *
 *   for a given pair of latitude/longitude.                  *
 *   (based on the formulas given in Ref. [1])                *
 *                                                            *
 *                                                            *
 * DEPENDENCIES:                                              *
 *   none                                                     *
 *                                                            *
 *                                                            *
 * REFERENCE:                                                 *
 * [1] LRIT/HRIT Global Specification                         *
 *     (CGMS 03, Issue 2.6, 12.08.1999)                       *
 *     for the parameters used in the program                 *
 * [2] MSG Ground Segment LRIT/HRIT Mission Specific          *
 *     Implementation, EUMETSAT Document,                     *
 *     (EUM/MSG/SPE/057, Issue 6, 21. June 2006).             *
 *                                                            *
 *                                                            *
 * MODIFICATION HISTORY:                                      *
 *   Version 1.02                                             *
 *  30.11.2011 added HRV to the calculation                   *
 *             Implemented with introducing CFAC/LFAC in      *
 *             function call                                  *
 *   Copyright(c) EUMETSAT 2005, 2009, 2011                   *
 *                                                            *
 *   Updated by ECMWF (31.10.2014) to take sublon as a        *
 *   parameter                                                *
 *                                                            *
 * INPUT:                                                     *
 *   latitude  (double) geographic Latitude of a point        *
 *                      [Degrees]                             *
 *   longitude (double) geographic Longitude of a point       *
 *                      [Degrees]                             *
 *   coff (int)   coefficient of the scalling function        *
 *                (see page 28, Ref [1])                      *
 *   loff (int)   coefficient of the scalling function        *
 *                (see page 28, Ref [1])                      *
 *   cfac  (real) image "spread" in the EW direction          *
 *   lfac  (real) image "spread" in the NS direction          *
 *   sublon (real) longitude of the sub-latitude point        *
 *                                                            *
 *                                                            *
 * OUTPUT:                                                    *
 *   row    (int) row-value of the wanted pixel               *
 *   column (int) column-value of the wanted pixel            *
 *                                                            *
 *************************************************************/

int geocoord2pixcoord(double latitude, double longitude, int coff, int loff, double cfac, double lfac, double sublon,
                      int* column, int* row) {
    int ccc = 0, lll = 0;

    double lati = 0.0, longi = 0.0;
    double c_lat = 0.0;
    double lat   = 0.0;
    double lon   = 0.0;
    double r1 = 0.0, r2 = 0.0, r3 = 0.0, rn = 0.0, re = 0.0, rl = 0.0;
    double xx = 0.0, yy = 0.0;
    double cc = 0.0, ll = 0.0;
    double dotprod = 0.0;

    lati  = latitude;
    longi = longitude;

    /* check if the values are sane, otherwise return error values */
    if (lati < -90.0 || lati > 90.0 || longi < -180.0 || longi > 180.0) {
        *row    = -999;
        *column = -999;
        return (-1);
    }


    /* convert them to radiants */
    lat = lati * PI / (double)180.;
    lon = longi * PI / (double)180.;

    /* calculate the geocentric latitude from the          */
    /* geograhpic one using equations on page 24, Ref. [1] */

    c_lat = atan(((double)0.993243 * (sin(lat) / cos(lat))));


    /* using c_lat calculate the length form the Earth */
    /* centre to the surface of the Earth ellipsoid    */
    /* equations on page 23, Ref. [1]                  */

    re = R_POL / sqrt(((double)1.0 - (double)0.00675701 * cos(c_lat) * cos(c_lat)));


    /* calculate the forward projection using equations on */
    /* page 24, Ref. [1]                                        */

    rl = re;
    r1 = SAT_HEIGHT - rl * cos(c_lat) * cos(lon - sublon);
    r2 = -rl * cos(c_lat) * sin(lon - sublon);
    r3 = rl * sin(c_lat);
    rn = sqrt(r1 * r1 + r2 * r2 + r3 * r3);


    /* check for visibility, whether the point on the Earth given by the */
    /* latitude/longitude pair is visible from the satellte or not. This */
    /* is given by the dot product between the vectors of:               */
    /* 1) the point to the spacecraft,                           */
    /* 2) the point to the centre of the Earth.                  */
    /* If the dot product is positive the point is visible otherwise it  */
    /* is invisible.                             */

    dotprod = r1 * (rl * cos(c_lat) * cos(lon - sublon)) - r2 * r2 - r3 * r3 * (pow((R_EQ / R_POL), 2));

    if (dotprod <= 0) {
        *column = -999;
        *row    = -999;
        return (-1);
    }

    /* the forward projection is x and y */
    xx = atan((-r2 / r1));
    yy = asin((-r3 / rn));

    /* convert to pixel column and row using the scaling functions on */
    /* page 28, Ref. [1]. And finding nearest integer value for them. */

    cc = coff + xx * pow(2., -16) * cfac;
    ll = loff + yy * pow(2., -16) * lfac;

    ccc = nint(cc);
    lll = nint(ll);

    *column = ccc;
    *row    = lll;

    return (0);
}


GribSatelliteInterpretor::GribSatelliteInterpretor() {}


GribSatelliteInterpretor::~GribSatelliteInterpretor() {}


/*
    GribSatelliteInterpretor::AdjustBadlyEncodedGribs
    Correct the information provided in the headers of certain satellite imagery that
    we have available. This is a very specific function.
*/
void GribSatelliteInterpretor::AdjustBadlyEncodedGribs(int satId, int chanId, long& nx, long& ny, long& dx, long& dy,
                                                       long& xp, long& yp, double& slon, long& functionCode) const {
    if (satId == 172 && slon == 140.0)  // MTSAT-2, pre-2015 data
    {
        dx = dy = 888;
        xp = yp = nx / 2;
        slon    = 145.0;
    }
    else if (satId == 54 && chanId == 2 && dx == 1179)  // Meteosat 7, channel 2
    {
        nx = ny = 900;
        dx = dy = 853;  // obtained through trial-and-error to get the best match with the coastlines
        xp = yp      = 450;
        functionCode = 1;  // wrongly encoded as 0
    }
    else if (satId == 54 && chanId == 3 && dx == 1179)  // Meteosat 7, channel 3
    {
        dx = dy = 1184;  // obtained through trial-and-error to get the best match with the coastlines
        xp = yp = 635;
    }
    else if (satId == 259 && chanId == 4 && dx == 1185)  // GOES-15 (West) channel 4
    {
        dx = dy = 880;  // obtained through trial-and-error to get the best match with the coastlines
        xp = yp = 450;
    }
    else if (satId == 57 && dx == 1732)  // MSG (Meteosat second generation), non-HRV channels
    {
        dx = dy = 1811;  // obtained through trial-and-error to get the best match with the coastlines
        xp = yp = 928;
    }
}

/*!
 Class information are given to the output-stream.
*/
void GribSatelliteInterpretor::print(ostream& out) const {
    out << "GribSatelliteInterpretor[";
    out << "]";
}
void GribSatelliteInterpretor::interpretAsMatrix(const GribDecoder& grib, Matrix** matrix, Matrix**) const {
    Timer timer("gribapi", " reproject satellite");
    MagLog::dev() << "GribRegularInterpretor::interpretAsMatrix"
                  << "\n";

    double altitude = grib.getDouble("NrInRadiusOfEarth");
    if (!altitude)
        altitude = 6610839.;

    // GRIB edition 1 needs to be divided by 10^6, GRIB 2 is already in the right units
    long edition = grib.getLong("edition");
    if (edition == 1)
        altitude *= 0.000001;

    long nx = grib.getLong("numberOfPointsAlongXAxis");
    long ny = grib.getLong("numberOfPointsAlongYAxis");
    long dx = grib.getLong("dx");
    long dy = grib.getLong("dy");
    // double offx = grib.getDouble("xCoordinateOfOriginOfSectorImage");
    // double offy = grib.getDouble("yCoordinateOfOriginOfSectorImage");
    double prj = 2 * asin(1 / altitude) / dx;
    double pri = 2 * asin(1 / altitude) / dy;
    long xp    = grib.getLong("XpInGridLengths");
    long yp    = grib.getLong("YpInGridLengths");


    double lao        = grib.getDouble("latitudeOfSubSatellitePointInDegrees") * TeCDR;
    double slon       = grib.getDouble("longitudeOfSubSatellitePointInDegrees");
    long sat          = grib.getLong("satelliteIdentifier");
    long chan         = grib.getLong("channelNumber");
    long functionCode = grib.getLong("functionCode");

    // correct bad GRIB headers that we know exist
    AdjustBadlyEncodedGribs(sat, chan, nx, ny, dx, dy, xp, yp, slon, functionCode);

    double lono      = slon * TeCDR;
    double prs       = altitude * TeEARTHRADIUS;
    double scn       = 0;
    double yaw       = grib.getDouble("orientationOfTheGrid");
    double target_dx = grib.regular_resolution_;  // resolution, in degrees of output lat/lon matrix
    double target_dy = grib.regular_resolution_;  // resolution, in degrees of output lat/lon matrix

    yaw = RAD(yaw / 1000);
    if (yaw < 0.)
        yaw += PI;
    else
        yaw -= PI;

    //  double resx = (double)(abs(( atan( tan(pri) * (altitude-1.) ) * TeEARTHRADIUS )));
    //  double resy = (double)(abs(( atan( tan(prj) * (altitude-1.) ) * TeEARTHRADIUS )));
    double resx = (double)(abs((atan(tan(pri) * (altitude - 1.)))));
    double resy = (double)(abs((atan(tan(prj) * (altitude - 1.)))));

    //  double west  = slon - 90 + (offx * resx);
    //  double east  = slon + 90 + ((offx + nx - 1) * resx);
    //  double north = -(offy * resy);
    //  double south = -(offy + ny - 1) * resy;

    double west  = -180;
    double east  = 180;
    double north = 90;
    double south = -90;

    long hasBitmap      = grib.getLong("bitmapPresent");
    double missingValue = 65535;
    if (hasBitmap)
        grib_set_double(grib.id(), "missingValue", missingValue);

    // get the array of values
    size_t nb = 0;
    grib_get_size(grib.id(), "values", &nb);
    vector<double> raster(nb);
    grib_get_double_array(grib.id(), "values", &raster.front(), &nb);


    // If value is temperature in degrees K then add 145 to pixel value
    double offset = (functionCode == 1) ? 145. : 0.;

    if (offset) {
        for (unsigned int i = 0; i < nb; i++) {
            if (!hasBitmap || raster[i] != missingValue)  // if not missing value
                raster[i] += offset;
        }
    }


    long nblon = ((east - west) / target_dx);    // + 1;
    long nblat = ((north - south) / target_dy);  // + 1;

    if (*matrix == 0)
        *matrix = new Matrix(nblat, nblon);


    double missing = INT_MAX;
    grib.setDouble("missingValue", missing);
    (*matrix)->missing(missing);


    MagLog::dev() << "NewAPI---> area[" << west << ", " << north << ", " << east << ", " << south << "]"
                  << "\n";

    double dlon = (east - west) / (nblon - 1);
    double dlat = (south - north) / (nblat - 1);

    MagLog::dev() << "calcul -->" << dlon << " (from->" << west << " to-->" << west + (nblon - 1) * dlon << ")" << endl;

    double x = west;
    for (int i = 1; i <= nblon; i++) {
        (*matrix)->columnsAxis().push_back(x);
        x = west + (i * dlon);
    }

    double y = north;
    for (int i = 1; i <= nblat; i++) {
        (*matrix)->rowsAxis().push_back(y);
        y = north + (i * dlat);
    }
    (*matrix)->setMapsAxis();


    double coff = xp;
    double loff = yp;

    double rx   = 2 * asin(1.0 / altitude) / dx;
    double cfac = (-65536.0 / (rx));
    double lfac = cfac;


    int k = 0;
    for (int j = 0; j < nblat; j++) {
        for (int i = 0; i < nblon; i++) {
            double val;
            int srcCol, srcRow;
            double lat = south - j * dlat;
            double lon = east - i * dlon;

            geocoord2pixcoord(lat, lon, coff, loff, cfac, lfac,
                              -lono,  // sub-satellite longitude in radians; unclear why we have to negate it
                              &srcCol, &srcRow);

            if (srcCol < 0 || srcCol >= nx || srcRow < 0 || srcRow >= ny)
                val = 65535;
            else
                val = raster[srcRow * nx + srcCol];

            (**matrix)[k++] = val;
        }
    }

    (*matrix)->missing(missingValue);
}


static SimpleObjectMaker<GribSatelliteInterpretor, GribInterpretor> gribsatellite("90");
