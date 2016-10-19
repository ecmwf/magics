/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \file SciMethods.h
    \brief Definition of SciMethods.
    \author Graphics Section, ECMWF

    Started: September 2011
*/

#ifndef SciMethods_H
#define SciMethods_H


#include "magics.h"

namespace magics
{
/*! Earth radius in km */
const double EarthRadiusInKm=6378.388;   //Earth radius in km 

/*! Computes the distance on Earth in km */
double geoDistanceInKm(double,double,double,double);

/*!  computes potential temperature */
double theta(double t, double p);

/*!  computes temperature form potential temperature */
double temperatureFromTheta(double th, double p);

/*!  computes pressure potential temperature */
double pressureFromTheta(double th, double t);

double tDew(double t, double rh);
double tWet(double td,double t,double p);
double thetaEq(double td,double t,double p);
double thetaEq(double t,double p);
double temperatureFromThetaEq(double thSat,double p);
double saturationVapourPressure(double t);
double mixingRatio(double t, double p);
double temperatureFromMixingRatio(double r, double p);
double temperatureLCL(double td,double t);
double pressureLCL(double td, double t, double p);
}

#endif
