/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// MvLocation.h,     vk 940901...
//               rev vk 950303

#ifndef MvLocation_DEFINED_
#define MvLocation_DEFINED_

#include "inc_iostream.h"

const double MISSING_LOC_VALUE = -99999.;


//_________________________________________________________________________ MvLocation
//! Class for geographical locations
/*! MvLocation is used to store latitude-longitude location values.
 *  Class also provides methods to calculate the distance to another
 *  geographical location
 */
class MvLocation
{
//! Output operator for MvLocation object
/*! The output is enclosed in parenthesis and latitude and longitude values
 *  are separated by a comma, for instance:
 * <PRE>
 *      MvLocation loc1(51.46,-1.33);
 *      MvLocation loc2(60.45,25.0);
 *      std::cout << "Locations are: " << loc1 << " and " << loc2 << std::endl;
 * </PRE>
 *  would output the following line:
 * <PRE>
 *      Locations are: (51.46,-1.33) and (60.45,25)
 * </PRE>
 */
 friend ostream& operator<< ( ostream& aStream, const MvLocation& aLocation );

 public:
	//! Constructor, location is assigned with missing values
	MvLocation() { fLatitude  = MISSING_LOC_VALUE;
	               fLongitude = MISSING_LOC_VALUE; }

	//! Constructor for latitude-longitude location
	MvLocation( double aLat, double aLong ) { set( aLat, aLong); }

   //! Sets new latitude-longitude location
   void  set( double aLat, double aLong );

   //! Checks that the stored location is a valid geographical point
   /*! Latitude value must be in interval [-90,90] and longitude
    *  value in interval [-360,360], in degrees.
    */
   bool  ok(){ return (fLatitude  <=  90 && fLatitude >=  -90 &&
                       fLongitude <= 360 && fLongitude >= -360); }

  //! Returns the latitude value
  double  latitude() const  { return fLatitude; }

  //! Alias to method latitude()
  double  y() const         { return fLatitude; }

  //! Returns the longitude value
  double  longitude() const { return fLongitude; }

  //! Alias to method longitude()
  double  x() const         { return fLongitude; }

  //! Returns the distance (in radians) to the given point
  double  distanceInRadians( const MvLocation& anOtherLocation ) const;

  //! Returns the distance (in degrees) to the given point
  double  distanceInDegrees( const MvLocation& anOtherLocation ) const;

  //! Returns the distance (in metres) to the given point
  double  distanceInMeters( const MvLocation& anOtherLocation ) const;

  //! Assignment operator
  MvLocation& operator= ( const MvLocation& aLoc );

 private:
  double fLatitude;
  double fLongitude;
};

//_________________________________________________________________________ MvArea
//! Class for geographical areas (squares on cylindrical projection)
/*! This is another incarnation of MvGeoBox class, used mainly by
 *  MvObsSetIterator. For other usage MvGeoBox is recommended over MvArea.
 */
class MvArea
{
 friend ostream& operator<< ( ostream& aStream, const MvArea& aArea );

 public:
         MvArea();
         MvArea( const MvLocation& aLoc1, const MvLocation& aLoc2 )
	                                                   { set( aLoc1, aLoc2 ); }

       void  set( const MvLocation& aLoc1, const MvLocation& aLoc2 );
       bool  inside( const MvLocation& aPoint ) const;
 MvLocation  lowerLeft( void ) const { return fLowerLeft; }
 MvLocation  upperRight( void ) const { return fUpperRight; }

  MvArea& operator= ( const MvArea& aLoc );

 private:
  MvLocation fLowerLeft;
  MvLocation fUpperRight;
};

//_____________________________________________________________________ MvXSectionLine
//! Class for cross section lines (straight lines on cylindrical projection)
/*! Class can be used to check if any given point is withing the allowed
 *  distance (max delta) from the given line, i.e. close enough to be
 *  considered to be within the line.
 */
class MvXSectionLine
{
 friend ostream& operator<< ( ostream& aStream, const MvXSectionLine& aXSectionLine );

 public:
	//! Empty constructor creates a missing line
	MvXSectionLine( void )
	              { fLocation1.set( MISSING_LOC_VALUE, MISSING_LOC_VALUE );
	                fLocation2.set( MISSING_LOC_VALUE, MISSING_LOC_VALUE );
	                fMaxDeltaInMeters = -1;
	              }

	//! Constructor, only points defined, no max delta given
	/*! Use method setMaxDelta() to set the maximum allowed distance from the line
	*/
	MvXSectionLine( const MvLocation& aLoc1, const MvLocation& aLoc2 )
	              { fLocation1 = aLoc1; fLocation2 = aLoc2; fMaxDeltaInMeters = -1; }

	//! Constructor, two points and max distance from the line
	MvXSectionLine( const MvLocation& aLoc1, const MvLocation& aLoc2, double aDelta )
	              { fLocation1 = aLoc1; fLocation2 = aLoc2; fMaxDeltaInMeters = aDelta; }

	//! Set a new line between points aLoc1 and aLoc2
	void  setLine( const MvLocation& aLoc1, const MvLocation& aLoc2 )
	             { fLocation1 = aLoc1; fLocation2 = aLoc2; }

	//! Return the current start point of the line
	MvLocation  startPoint( void ) const { return fLocation1; }

	//! Return the current end point of the line
	MvLocation  endPoint( void ) const   { return fLocation2; }

	//! Set the maximum allowed distance from the line
	void  setMaxDelta( double aDelta ) { fMaxDeltaInMeters = aDelta; }

	//! Return the current maximum allowed distance from the line
	double  maxDelta( void ) const { return fMaxDeltaInMeters; }

	//! Returns true if the given point is within the given proximity of the line
	bool  withinDelta( const MvLocation& aLocation ) const;

	//! Returns the distance of the given point from the cross section line (in degrees)
	double  deltaInDegrees( const MvLocation& aLocation ) const;

	//! Returns the distance of the given point from the cross section line (in meters)
	double  deltaInMeters( const MvLocation& aLocation ) const;

	//! Assignment operator
	MvXSectionLine& operator= ( const MvXSectionLine& anXLine );

 private:
	//! Returns the nearest point on the line through end points
	MvLocation  nearestPointOnXLine( const MvLocation& aLocation ) const;

	//! Checks that the closest point on the line is between end points
	bool  insideXLine( const MvLocation& aLocation ) const;

 private:
  MvLocation  fLocation1;
  MvLocation  fLocation2;
      double  fMaxDeltaInMeters;
};

#endif
// MvLocation_DEFINED_
