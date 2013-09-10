/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvLocation.cc,     vk 940901...
//                rev vk 011025

// classes:  MvLocation, MvArea, MvXSectionLine


#include <math.h>
//#include "Metview.h"
#include "MvLocation.h"

//_____________________________________________________________________ set
void
MvLocation :: set( double aLat, double aLong )
{
/*--- to check or not to check: XSection computes values outside the real world!!!
   if( aLat != MISSING_LOC_VALUE && ( aLat > 90. || aLat < -90. ) )
   {
     cerr << " >>> MvLocation::MvLocation: strange latitude value " << aLat << endl;
     fLatitude = MISSING_LOC_VALUE;
   }
   else
---*/
   fLatitude = aLat;

/*---
   if( aLong != MISSING_LOC_VALUE && ( aLong > 360. || aLong < -360. ) )
   {
     cerr << " >>> MvLocation::MvLocation: strange longitude value " << aLong << endl;
     fLongitude = MISSING_LOC_VALUE;
   }
   else
---*/
   fLongitude = aLong;
}

//______________________________________________________________ distanceInRadians
double
MvLocation :: distanceInRadians( const MvLocation& anOtherLocation ) const
{
   const double cDEG2RAD = M_PI / 180.0;

   double lat1 = latitude() * cDEG2RAD;
   double lat2 = anOtherLocation.latitude() * cDEG2RAD;
   double lon1 = longitude() * cDEG2RAD;
   double lon2 = anOtherLocation.longitude() * cDEG2RAD;

   //-- from: http://williams.best.vwh.net/avform.htm (020815/vk) --
   double d = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));

   return d;
}

//_____________________________________________________________ distanceInDegrees
double
MvLocation :: distanceInDegrees( const MvLocation& anOtherLocation ) const
{
   return distanceInRadians( anOtherLocation ) * 180.0 / M_PI;
}

//_____________________________________________________________ distanceInMeters
double
MvLocation :: distanceInMeters( const MvLocation& anOtherLocation ) const
{
  const double cR2NM = 180.0*60.0/M_PI;  //-- radians -> nautical miles
  const double cNM2M = 1852;             //-- nautical miles -> metres

  double nm = distanceInRadians( anOtherLocation ) * cR2NM;
  return nm * cNM2M;;
}

//_____________________________________________________________ operator=
MvLocation&
MvLocation :: operator= ( const MvLocation& aLoc )
{
   set( aLoc.latitude(), aLoc.longitude() );
   return *this;
}
//_____________________________________________________________ operator<<

ostream& operator<< ( ostream& aStream, const MvLocation& aLocation )
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

//____________________________________________________________________________
//============================================================================ MvArea
//____________________________________________________________________________

MvArea :: MvArea( void )
{
   MvLocation myLocation;
   set( myLocation, myLocation );
}
//____________________________________________________________________

void
MvArea :: set( const MvLocation& aLocation1, const MvLocation& aLocation2 )
{
   double y1 = aLocation1.latitude();
   double x1 = aLocation1.longitude();
   double y2 = aLocation2.latitude();
   double x2 = aLocation2.longitude();
   if( y1 > y2 )
   {
      double yy = y1; y1 = y2; y2 = yy;
   }
   if( x1 > x2 )
   {
      double xx = x1; x1 = x2; x2 = xx;
   }
   fLowerLeft.set( y1, x1 );
   fUpperRight.set( y2, x2 );
}
//____________________________________________________________________

bool
MvArea :: inside( const MvLocation& aPoint ) const
{
   if( aPoint.latitude() >= fLowerLeft.latitude() &&
       aPoint.latitude() <= fUpperRight.latitude() &&
       aPoint.longitude() >= fLowerLeft.longitude() &&
       aPoint.longitude() <= fUpperRight.longitude()
      )
     return true;
   else
     return false;
}
//____________________________________________________________________

MvArea&
MvArea :: operator= ( const MvArea& anArea )
{
   set( anArea.lowerLeft(), anArea.upperRight() );
   return *this;
}
//____________________________________________________________________

ostream& operator<< ( ostream& aStream, const MvArea& anArea )
{
   aStream << anArea.lowerLeft() << "-" << anArea.upperRight();
   return aStream;
}

//______________________________________________________________________
//====================================================================== MvXSectionLine
//______________________________________________________________________

//_____________________________________________________________ WithinDelta
bool
MvXSectionLine :: withinDelta( const MvLocation& aLocation ) const
{
   //-- check that max delta has been set
   if( fMaxDeltaInMeters < 0 )
      return false;

   //-- check distance from the line going through end points
   if( deltaInMeters( aLocation ) > fMaxDeltaInMeters )
      return false;

   //--- check that the distance is from the line between end points
   return insideXLine( aLocation );
}
//_____________________________________________________________ InsideXLine
bool
MvXSectionLine :: insideXLine( const MvLocation& aLocation ) const
{
   MvLocation myLocation = nearestPointOnXLine( aLocation );
   MvArea myArea( fLocation1, fLocation2 );
   return myArea.inside( myLocation );
}
//_____________________________________________________________ NearestPointOnXLine
// Q&D approximation for distance of a point from an XSection
// line, in degrees true at equator...
//--------
// WARNING: calculates distance from a line going through end
//          points of XSectionLine, NOT ONLY between points!
//-------------------------------------------------------------
MvLocation
MvXSectionLine :: nearestPointOnXLine( const MvLocation& aLocation ) const
{
   MvLocation myNearestPointOnXLine;

   double dy = fLocation1.latitude() - fLocation2.latitude();
   double dx = fLocation1.longitude() - fLocation2.longitude();

   if( dx == 0 )            //-- vertical line --
   {
      myNearestPointOnXLine.set( aLocation.latitude(), fLocation1.longitude() );
   }
   else if( dy == 0 )       //-- horizontal line --
      {
         myNearestPointOnXLine.set( fLocation1.latitude(), aLocation.longitude() );
      }
      else                  //-- just a line --
      {
         // calculate coefficients for the Xsection line: y=a1*x+b1
         double a1 = dy / dx;
         double b1 = fLocation1.latitude() - a1 * fLocation1.longitude();

         // define perpendicular line thru aLocation: y=a2*x+b2
         double a2 = - 1. / a1;
         double b2 = aLocation.latitude() - a2 * aLocation.longitude();

         double x = ( b2 - b1 ) / ( a1 - a2 );
         double y = a1 * x + b1;
         myNearestPointOnXLine.set( y, x );
      }

   //-- WARNING: does not check that nearest point is inside XSectionLine !!!!
   return  myNearestPointOnXLine;
}
//_____________________________________________________________ DeltaInDegrees
double
MvXSectionLine :: deltaInDegrees( const MvLocation& aLocation ) const
{
   return aLocation.distanceInDegrees( nearestPointOnXLine( aLocation ) );
}
//_____________________________________________________________ DeltaInMeters
// Q&D approximation for delta, uses DeltaInDegrees
//-------------------------------------------------------------
double
MvXSectionLine :: deltaInMeters( const MvLocation& aLocation ) const
{
   //-- Q&D formula, out of my old used brains, unchecked!!!! (vk 940901) --
   double degreeIntoMeters = 6370.*1000. * 2. * M_PI / 360.;
   return deltaInDegrees( aLocation ) * degreeIntoMeters;
}
//_____________________________________________________________ operator=
MvXSectionLine&
MvXSectionLine :: operator= ( const MvXSectionLine& anXLine )
{
   setLine( anXLine.startPoint(), anXLine.endPoint() );
   setMaxDelta( anXLine.maxDelta() );
   return *this;
}
//_____________________________________________________________ operator<<
// output format: "(lat1,long1)-(lat2,long2)/delta"
//-------------------------------------------------

ostream& operator<< ( ostream& aStream, const MvXSectionLine& anXLine )
{
   aStream << anXLine.startPoint() << "-" << anXLine.endPoint()
           << "/" << anXLine.fMaxDeltaInMeters; // << endl;
   return aStream;
}

