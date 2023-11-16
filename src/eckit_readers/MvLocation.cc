/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

#include <cmath>
#include "MvLocation.h"

//===================================================
//
// MvLocation
//
//===================================================

const double MvLocation::cRadian     = 180. / M_PI;
const double MvLocation::cDegree     = 1 / MvLocation::cRadian;
const double MvLocation::cRadianToMetre = 1852 *180.0 * 60.0 / M_PI;
const double MvLocation::cMetreToRadian = 1./MvLocation::cRadianToMetre;


MvLocation& MvLocation::operator=(const MvLocation& aLoc)
{
    set(aLoc.latitude(), aLoc.longitude());
    return *this;
}

void MvLocation::set(double aLat, double aLong)
{  
    lat_ = aLat;
    lon_ = aLong;
}

void MvLocation::ensureLongitudeBelow360()
{
    while (lon_ >= 360) {
        lon_ -= 360;
    }
}

// compute the cosine of the angular distance between the current and the other location
double MvLocation::cosOfDistance(double aLat, double aLon) const
{
    // short-circuit if the two points are the same, because floating-point
    // imprecisions can cause a 'nan' on the next calculation
    if ((lat_ == aLat) && (lon_ == aLon)) {
        return 1.;
    }

    double lat1 = degToRad(lat_);
    double lat2 = degToRad(aLat);
    double dlon = degToRad(lon_ - aLon);

     //-- from: http://williams.best.vwh.net/avform.htm
    return std::sin(lat1) * std::sin(lat2) + std::cos(lat1) * std::cos(lat2) * std::cos(dlon);
}

// compute the cosine of the angular distance between the current and the other location
double MvLocation::cosOfDistance(const MvLocation& other) const
{
    return cosOfDistance(other.latitude(), other.longitude());
}

// compute the angular distance in radians between the current and the other location
double MvLocation::distanceInRadians(const MvLocation& other) const
{
    return std::acos(cosOfDistance(other));
}

// compute the angular distance in degrees between the current and the other location
double MvLocation::distanceInDegrees(const MvLocation& other) const
{
    return radToDeg(distanceInRadians(other));
}

// compute the distance in metres on the surface of Earth
// between the current and the other location
double MvLocation::distanceInMeters(const MvLocation& other) const
{
    return radiansToMetres(distanceInRadians(other));
}

std::ostream& operator<<(std::ostream& aStream, const MvLocation& aLocation)
{
    aStream << "(" << aLocation.latitude() << "," << aLocation.longitude() << ")";
    /*--- How to get a constant field width + constant nr of decimal digits!?
   aStream << "(";
   aStream.width( 6 ); aStream.fill( ' ' );
   aStream << aLocation.latitude() << ",";
   aStream.width( 7 ); aStream.fill( ' ' );
   aStream << aLocation.longitude() << ")";
---*/
    return aStream;
}

//===================================================
//
// MvLocationHub
//
//===================================================

MvLocationHub& MvLocationHub::operator=(const MvLocationHub& aLoc)
{
    set(aLoc.latitude(), aLoc.longitude());
    cosLat_ = aLoc.cosLat_;
    sinLat_ = aLoc.sinLat_;
    return *this;
}

double MvLocationHub::cosOfDistance(double aLat, double aLon) const
{
    if (sinLat_ < -100) {
        sinLat_ = std::sin(degToRad(lat_));
        cosLat_ = std::cos(degToRad(lat_));
    }

    // short-circuit if the two points are the same, because floating-point
    // imprecisions can cause a 'nan' on the next calculation
    if (lat_ == aLat && lon_ == aLon) {
        return 1.;
    }

    double lat2 = degToRad(aLat);
    double dLon = degToRad(lon_ - aLon);

     //-- from: http://williams.best.vwh.net/avform.htm
    return sinLat_* std::sin(lat2) + cosLat_ * std::cos(lat2) * std::cos(dLon);
}

//____________________________________________________________________________
//============================================================================ MvArea
//____________________________________________________________________________

MvArea ::MvArea()
{
    MvLocation myLocation;
    set(myLocation, myLocation);
}
//____________________________________________________________________

void MvArea ::set(const MvLocation& aLocation1, const MvLocation& aLocation2)
{
    double y1 = aLocation1.latitude();
    double x1 = aLocation1.longitude();
    double y2 = aLocation2.latitude();
    double x2 = aLocation2.longitude();
    if (y1 > y2) {
        double yy = y1;
        y1        = y2;
        y2        = yy;
    }
    if (x1 > x2) {
        double xx = x1;
        x1        = x2;
        x2        = xx;
    }
    fLowerLeft.set(y1, x1);
    fUpperRight.set(y2, x2);
}
//____________________________________________________________________

bool MvArea ::inside(const MvLocation& aPoint) const
{
    if (aPoint.latitude() >= fLowerLeft.latitude() &&
        aPoint.latitude() <= fUpperRight.latitude() &&
        aPoint.longitude() >= fLowerLeft.longitude() &&
        aPoint.longitude() <= fUpperRight.longitude())
        return true;
    else
        return false;
}
//____________________________________________________________________

MvArea&
MvArea ::operator=(const MvArea& anArea)
{
    set(anArea.lowerLeft(), anArea.upperRight());
    return *this;
}
//____________________________________________________________________

std::ostream& operator<<(std::ostream& aStream, const MvArea& anArea)
{
    aStream << anArea.lowerLeft() << "-" << anArea.upperRight();
    return aStream;
}

//______________________________________________________________________
//====================================================================== MvXSectionLine
//______________________________________________________________________

//_____________________________________________________________ WithinDelta
bool MvXSectionLine ::withinDelta(const MvLocation& aLocation) const
{
    //-- check that max delta has been set
    if (fMaxDeltaInMeters < 0)
        return false;

    //-- check distance from the line going through end points
    if (deltaInMeters(aLocation) > fMaxDeltaInMeters)
        return false;

    //--- check that the distance is from the line between end points
    return insideXLine(aLocation);
}
//_____________________________________________________________ InsideXLine
bool MvXSectionLine ::insideXLine(const MvLocation& aLocation) const
{
    MvLocation myLocation = nearestPointOnXLine(aLocation);
    MvArea myArea(fLocation1, fLocation2);
    return myArea.inside(myLocation);
}
//_____________________________________________________________ NearestPointOnXLine
// Q&D approximation for distance of a point from an XSection
// line, in degrees true at equator...
//--------
// WARNING: calculates distance from a line going through end
//          points of XSectionLine, NOT ONLY between points!
//-------------------------------------------------------------
MvLocation
MvXSectionLine ::nearestPointOnXLine(const MvLocation& aLocation) const
{
    MvLocation myNearestPointOnXLine;

    double dy = fLocation1.latitude() - fLocation2.latitude();
    double dx = fLocation1.longitude() - fLocation2.longitude();

    if (dx == 0)  //-- vertical line --
    {
        myNearestPointOnXLine.set(aLocation.latitude(), fLocation1.longitude());
    }
    else if (dy == 0)  //-- horizontal line --
    {
        myNearestPointOnXLine.set(fLocation1.latitude(), aLocation.longitude());
    }
    else  //-- just a line --
    {
        // calculate coefficients for the Xsection line: y=a1*x+b1
        double a1 = dy / dx;
        double b1 = fLocation1.latitude() - a1 * fLocation1.longitude();

        // define perpendicular line thru aLocation: y=a2*x+b2
        double a2 = -1. / a1;
        double b2 = aLocation.latitude() - a2 * aLocation.longitude();

        double x = (b2 - b1) / (a1 - a2);
        double y = a1 * x + b1;
        myNearestPointOnXLine.set(y, x);
    }

    //-- WARNING: does not check that nearest point is inside XSectionLine !!!!
    return myNearestPointOnXLine;
}
//_____________________________________________________________ DeltaInDegrees
double
MvXSectionLine ::deltaInDegrees(const MvLocation& aLocation) const
{
    return aLocation.distanceInDegrees(nearestPointOnXLine(aLocation));
}
//_____________________________________________________________ DeltaInMeters
// Q&D approximation for delta, uses DeltaInDegrees
//-------------------------------------------------------------
double
MvXSectionLine ::deltaInMeters(const MvLocation& aLocation) const
{
    //-- Q&D formula, out of my old used brains, unchecked!!!! (vk 940901) --
    double degreeIntoMeters = 6370. * 1000. * 2. * M_PI / 360.;
    return deltaInDegrees(aLocation) * degreeIntoMeters;
}
//_____________________________________________________________ operator=
MvXSectionLine&
MvXSectionLine ::operator=(const MvXSectionLine& anXLine)
{
    setLine(anXLine.startPoint(), anXLine.endPoint());
    setMaxDelta(anXLine.maxDelta());
    return *this;
}
//_____________________________________________________________ operator<<
// output format: "(lat1,long1)-(lat2,long2)/delta"
//-------------------------------------------------

std::ostream& operator<<(std::ostream& aStream, const MvXSectionLine& anXLine)
{
    aStream << anXLine.startPoint() << "-" << anXLine.endPoint()
            << "/" << anXLine.fMaxDeltaInMeters;  // << std::endl;
    return aStream;
}
