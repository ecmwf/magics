/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeProjection.h
    \brief This file contains support do deal with geographical projections
*/

#ifndef  __TERRALIB_INTERNAL_PROJECTION_H
#define  __TERRALIB_INTERNAL_PROJECTION_H

#include "TeCoord2D.h"
#include "TeDefines.h"
#include "TeDatum.h"

#include <stdio.h>
#include <map>
#include <string>

using namespace std;

//! Earth hemispheres 
enum TeHemisphere
{ TeNORTH_HEM, TeSOUTH_HEM };

//! Number of supported projections in TerraLib
const int NUM_PROJ = 11; 

//! Set of informations required by projections
struct TL_DLL TeProjInfo 
{
	int hasUnits;
	int hasLon0;
	int hasLat0;
	int hasStlat1;
	int hasStlat2;
	int hasScale;
	int hasOffx;
	int hasOffy;
};

//! A map from name of projections to a set of informations that it requires
typedef map<string,TeProjInfo> TeProjInfoMap;

//! Returns the set of informations required by a given projection
TL_DLL TeProjInfo TeProjectionInfo ( const string& projName  );

TL_DLL const char** TeGetProjInfo();

// ============ PROJECTION PARAMETERS ===============

class TeProjection;

//FAMI Added Satellite parameters
//! Set of parameters that define a geographical projection
struct TL_DLL TeProjectionParams
{
	string	name;	     //!< projection name
	TeDatum datum;       //!< spheroid
        double	lon0;	     //!< Longitude of origin (rad)
	double  lat0;	     //!< Latitude of origin (rad)  
	double	offx;	     //!< X (projection coordinate) offset (m)
	double	offy;	     //!< Y (projection coordinate) offset (m)
	double	stlat1;	     //!< First standard parallel (rad)
	double  stlat2;      //!< Second standard paralel (rad)
	string  units;       //!< units
	double  scale;       //!< projection scale (used in UTM)
	TeHemisphere hemisphere; //!< Hemisphere
	double  pri;         //!< Sensor angle resolution along y axis (rad) (used in Satellite)
	double  prj;         //!< Sensor angle resolution along x axis (rad) (used in Satellite)
	double  pis;         //!< Y-coordinate of sub-satellite point (used in Satellite)
	double  pjs;         //!< X-coordinate of sub-satellite point (used in Satellite)
	double  prs;         //!< Radius of satellite orbit (m) (used in Satellite)
	double  pscn;        //!< Scanning mode: 0-WE/NS, 1-SN/EW (used in Satellite)
	double  pyaw;        //!< Grid orientation, i.e., angle in radians between the increasing y axis and the meridian of the sub-satellite point along the direction of increasing latitude (used in Satellite)

};

//========== PROJECTION FACTORY
//! A factory of projections
class TL_DLL TeProjectionFactory 
{
public:
	static TeProjection* make( const TeProjectionParams& );
};

//!  Provides methods that are required to handle all map projection definitions and georeferencing of satellite images.
/*!
  Specifies earth and projection parameters that represent a common
  ground in terms of defining conventional map projections, navigating
  on low-resolution images of geostationary satellites.
*/
class TL_DLL TeProjection 
{
protected:
	string	GPname;	             // projection name
	TeDatum GPdatum;             // spheroid
	double	GPlon0;	             // Longitude of origin (rad)
	double  GPlat0;	             // Latitude of origin (rad)  
	double	GPoffx;	             // X (projection coordinate) offset (m)
	double	GPoffy;	             // Y (projection coordinate) offset (m)
	double	GPstlat1;            // First standard parallel (rad)
	double  GPstlat2;            // Second standard parallel (rad)
	string  GPunits;             // units
	double  GPscale;             // scale (used for UTM)
	TeHemisphere  GPhemisphere;  // Hemisphere
	TeProjection* GPdestination; // destination projection
	int	      GPid;          // id

	//! Changes planimetic datum
	/*!	Computes changes in geodetic coordinates due to
		planimetric datum changes. First, the method finds
		geocentric cartesian coordinates on the initial datum,
		then applies datum shifts, and finally computes the
		new geodetic coordinates on the final datum. The new
		geodetic latitude is computed iteractively, the old
		geodetic latitude being used as initial guess.
		
		\param	x	Initial longitude (rad);
		\param	y	Initial latitude (rad).
		\return x	Final longitude (rad);
		\return y	Final latitude (rad).
		
		\note
			x must be a valid longitude ([0,pi] or [0,-pi]) and
			y must be a valid latitude ([0,pi/2] or [0,-pi/2]).
	*/
	void 	ChangeLL (double &x, double &y); 

public:

	//! Normal constructor
	/*!	
		Initializes projection parameters 
		\param name			projection name
		\param datum		spheroid
		\param lon0			longitude of origin (in radians)
		\param lat0			latitude of origin (in radians)  
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param stlat1		first standard parallel (in radians)
		\param stlat2		second standard parallel (in radians)
		\param units		projection unit
		\param scale		scale (used for UTM)
		\param hem			hemisphere
	*/
	TeProjection ( const string& name, const TeDatum& datum,
		double lon0 = 0., double lat0=0., double offx = 0., double offy = 0.,
		double stlat1 = 0., double stlat2 = 0., 
		const string units = "Meters",
		double scale = 1., TeHemisphere hem = TeSOUTH_HEM):
		GPname   ( name ),
		GPdatum  ( datum ),
	    GPlon0   ( lon0 ),
	    GPlat0   ( lat0  ),
	    GPoffx   ( offx  ),
	    GPoffy   ( offy ),
	    GPstlat1 ( stlat1 ),
	    GPstlat2 ( stlat2 ),
	    GPunits  ( units ),
	    GPscale  ( scale ),
	    GPhemisphere ( hem ),
		GPdestination (0),
		GPid(0)
		{}

		TeProjection ():
		GPname   ( "NoProjection" ),
	    GPlon0   ( 0. ),
	    GPlat0   ( 0. ),
	    GPoffx   ( 0. ),
	    GPoffy   ( 0. ),
	    GPstlat1 ( 0. ),
	    GPstlat2 ( 0. ),
	    GPunits  ( "Units" ),
	    GPscale  ( 1 ),
	    GPhemisphere ( TeSOUTH_HEM ),
		GPdestination (0),
		GPid(0)
	{}

	//! Copy Constructor
	TeProjection(const TeProjection&);

	//! Operator =
	TeProjection& operator=(const TeProjection&);

	//!	Destructor.
	virtual ~TeProjection (){}

	//! Returns the projection name;
	string& name() 
	{ return GPname; }

	//! Returns the projection datum
	TeDatum datum()
	{ return GPdatum; }

	//! Sets the datum associated to the projection 
	void setDatum(const TeDatum& datum)
	{	GPdatum = datum; }

	//! Return the projection units 
	string& units()
	{ return GPunits; }

	//! Return the longitude of origin ( in rad)
	double	lon0() { return GPlon0;	}
	
	//! Return the Latitude of origin (rad)
	double  lat0() { return GPlat0;	}	  

	//! Return  X (projection coordinate) offset (m)
	double	offX() { return GPoffx; }	 
    
	//! Return  Y (projection coordinate) offset (m)
	double	offY() { return GPoffy;	 }
    
	//!  Return First standard parallel (rad)
	double	stLat1() { return GPstlat1; }
	
	//! Return the second standard parallel (rad)
	double  stLat2() { return GPstlat2; }
	
	//! Return the scale (used for UTM)
	double  scale()  { return GPscale;   }

	//! Return the  Hemisphere
	TeHemisphere hemisphere() {	return GPhemisphere; }
		
	// Returns a the parameters of this projection
	virtual TeProjectionParams params () const; //FAMI

	bool operator== (const TeProjection& proj);

	//!	Pure virtual method that transforms geodetic into projection coordinates
	/*!		
		This method is implemented for each available
		projection class and represents the so-called
		direct formulas, which compute projection 
		coordinates from geodetic coordinates.
		 
		\param	p 	Geodetic coordinates (radian).
		\return p  	Projection coordinates (m).
		\note
			Geodetic coordinates must be a valid latitude
			([0,pi/2] or [0,-pi/2]) and a valid longitude
			([0,pi] or [0,-pi]).
	*/
	virtual	TeCoord2D LL2PC (TeCoord2D& p) = 0;
	virtual	void	  LL2PC (ostream&) const {} //FAMI

	//! Pure virtual method that transforms projection into geodetic coordinates.
	/*!
		This method is implemented for each available
		projection class and represents the so-called
		inverse formulas, which compute geodetic
		coordinates from projection coordinates.
		\param p 	Projection coordinates (m).
		\return p	Geodetic coordinates (rad).
		\note X and Y projection coordinates must be both valid,
		 within the typical range of each projection class.
	*/
	virtual	TeCoord2D	PC2LL (TeCoord2D& p) = 0;

	virtual	void LL2PC (double, double, double&, double&) const {printf("\nWRONG"); return;} //FAMI
	virtual	void PC2LL (double, double, double&, double&) {printf("\nWRONG"); return;} //FAMI

	//!	Sets the projection to which a Latitude/Longitude value will be generated by the current projection by calling changeLL in PC2LL method
	void setDestinationProjection (TeProjection* proj)
	{ GPdestination = proj; }
	
	//! Concrete method that prints information about a projection
	void print ( FILE* file_ );

	//! Concrete method that prints information about a projection in a string
	string describe ();

	//! Get projection unique id in the database
	int	id() { return GPid;}

	//! Set projection unique id in the database
	void id(int i) { GPid = i;}
};


//!  Provides methods that are required to handle the UTM map projection.
/*!
	  Specifies methods that are necessary to establish the relation between
	  geodetic and UTM coordinates. UTM is a conformal projection system that
	  uses the planimetric datum Sad69 or Corrego Alegre  (Hayford ellipsoid).

	 \sa TeCoord2D TeDatum TeProjection.
*/
class TL_DLL TeUtm : public TeProjection
{
public:

	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param long0		longitude of origin (in radians)
		\param lat0		latitude of origin (in radians)  
		\param offx		X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param units		projection unit
		\param scale		scale (used for UTM)
		\param hemisphere	hemisphere
	*/
	TeUtm ( const TeDatum& datum, double long0, double lat0 = 0., 
		     double offx = 500000., double offy = 10000000., 
			 const string& units = "Meters",
			 double scale = 0.9996, TeHemisphere hemisphere = TeSOUTH_HEM ):
	TeProjection ( "UTM", datum, long0, lat0, offx, offy, 0., 0., units, scale,
		hemisphere)

	{
		if ( hemisphere == TeNORTH_HEM )
			GPoffy = 0.;
	}

	//! Destructor
	~TeUtm () {}

	//! This implementation of a pure virtual method defined in Projection transforms geodetic into UTM coordinates.
	/*!		 
		\param	p	Geodetic coordinates (rad).
		\return	p:	UTM coordinates (m).
		\note
			Geodetic coordinates must be a valid latitude
			([0,pi/2] or [0,-pi/2]) and a valid longitude
			([0,pi] or [0,-pi]). Conventional UTM offsets
			(500,000 m and 10,000,000 m) are always added
			to the resulting projection coordinates.
	*/
	virtual TeCoord2D LL2PC(TeCoord2D& p);

	//!	This implementation of a pure virtual method defined in
	/*	Projection transforms UTM into geodetic coordinates.
		\param p:	UTM coordinates (m).
		\return p:	Geodetic coordinates (rad).
		\note
		X and Y UTM coordinates must be both valid, within
		their typical range. Conventional UTM offsets are
		handled in this method, and therefore must not be 
		previously subtracted.
	*/
	virtual TeCoord2D PC2LL(TeCoord2D& p);
};

//!  Provides methods that are required to handle Lambert conformal conic map projection..
/*!
  Specifies methods that are necessary to establish the relation between
  geodetic and Lambert conformal conic coordinates. 
  \sa TeCoord2D TeDatum TeProjection.
*/
class TL_DLL TeLambertConformal : public TeProjection
{
public :
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param long0		longitude of origin (in radians)
		\param lat0		latitude of origin (in radians)  
		\param offx		X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param stlat1		first standard parallel (in radians)
		\param stlat2		second standard parallel (in radians)
		\param units		projection unit
	*/
	TeLambertConformal ( const TeDatum& datum, 
		                 double long0,
			             double lat0, 
						 double offx, 
						 double offy,
						 double stlat1, 
						 double stlat2, 
						 const string& units = "Meters" ):
	TeProjection ( "LambertConformal", datum, long0, lat0, offx, offy, stlat1, stlat2, units)
	{}

	//! Destructor
	~TeLambertConformal () {}


	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual	TeCoord2D PC2LL(TeCoord2D& p);
};

//!  Provides methods that are required to handle the Mercator map projection.
/*! 
  Specifies methods that are necessary to establish the relation between
  geodetic and Mercator coordinates.  
*/
class TL_DLL TeMercator : public TeProjection
{

public:
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param long0			longitude of origin (in radians)
		\param lat0			latitude of origin (in radians)  
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param stlat1		first standard parallel (in radians)
		\param units		projection unit
	*/
	TeMercator ( const TeDatum& datum,  
		         double long0, 
				 double lat0 = 0.,
				 double offx = 0., 
				 double offy = 0.,
				 double stlat1 = 0.,
				 const string& units = "Meters"): 
	TeProjection ( "Mercator", datum, long0, lat0, offx, offy, stlat1, 0., units )
	{}

	//! Destructor
	~TeMercator () {}

	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual TeCoord2D PC2LL(TeCoord2D& p);

};



//! Provides methods that are required to handle the Polyconic map projection.
/*!  
	Specifies methods that are necessary to establish the relation between
    geodetic and Polyconic coordinates. Polyconic is a projection system that
    is neither conformal nor equal-area. 
*/  
class TL_DLL TePolyconic : public TeProjection
{
public:
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param long0			longitude of origin (in radians)
		\param lat0			latitude of origin (in radians)  
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param units		projection unit
	*/
	TePolyconic (const TeDatum& datum,  
		         double long0, 
				 double lat0 = 0.,
				 double offx = 0., 
				 double offy = 0.,
				 const string& units = "Meters"): 
	TeProjection ( "Polyconic", datum, long0, lat0, offx, offy, 0., 0., units )
	{}  

	//! Destructor
	~TePolyconic () {}


	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual TeCoord2D PC2LL(TeCoord2D& p);
};

//!  Provides methods that are required to handle the Equidistant Cylindrical  map projection.
class TL_DLL TeLatLong : public TeProjection
{

public :
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param units		projection unit
	*/
	TeLatLong( const TeDatum& datum, const string& units = "DecimalDegrees" ):
	   TeProjection ( "LatLong", datum, 0., 0., 0., 0., 0., 0., units )
	{}

	//! Destructor
	~TeLatLong () {}

	//! Returns the same coordinate
	virtual TeCoord2D LL2PC(TeCoord2D& p);

	//! Returns the same coordinate
	virtual TeCoord2D PC2LL(TeCoord2D& p);

	virtual void LL2PC(double xi, double yi, double& xo, double& yo) const; //FAMI
	virtual void PC2LL(double xi, double yi, double& xo, double& yo); //FAMI

};

//!  Provides methods that are required to handle the Albers Conic map  projection.
/*
  Specifies methods that are necessary to establish the relation between
  geodetic and Albers Conic coordinates. Albers Conic is an equal-area
  projection system. 
*/
class TL_DLL TeAlbers : public TeProjection
{
public:
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param lon0			longitude of origin (in radians)
		\param lat0			latitude of origin (in radians)  
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param stlat1		first standard parallel (in radians)
		\param stlat2		second standard parallel (in radians)
		\param units		projection unit
	*/
	TeAlbers( const TeDatum& datum, 
		      double lon0,
			  double lat0, 
			  double offx, 
			  double offy,
			  double stlat1, 
			  double stlat2, 
			  const string& units = "Meters" ):
		TeProjection ( "Albers", datum, lon0, lat0, offx, offy, stlat1, stlat2, units )
	{} 

	//! Destructor
	~TeAlbers () { }

	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual TeCoord2D PC2LL(TeCoord2D& p);
};

//!  Provides methods that are required to handle the Miller map projection.
/*!
  Specifies methods that are necessary to establish the relation between
  geodetic and Miller coordinates. Miller is a projection system that
  is neither conformal nor equal-area."
*/
class TL_DLL TeMiller : public TeProjection	
{
public:
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param long0		longitude of origin (in radians)
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param units		projection unit
	*/
	TeMiller ( const TeDatum& datum,  
		         double long0, 
				 double offx = 0., 
				 double offy = 0.,
				 const string& units = "Meters"): 
	TeProjection ( "Miller", datum, long0, 0., offx, offy, 0., 0., units )
	{}

	//! Destructor
	~TeMiller () {}

	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual TeCoord2D PC2LL(TeCoord2D& p);

};

//! Provides methods that are required to handle the Sinusoidal map projection.
/*!
  Specifies methods that are necessary to establish the relation between
  geodetic and Sinusoidal coordinates. Sinusoidal is a projection system that
  is equal-area. Being not an interrupted form, 
  this implementation assumes a single central meridian. Spheroid options 
  can be redefined by editing the file "TeDatum.cpp"
*/
class TL_DLL TeSinusoidal : public TeProjection	
{
public:

//!	Constructor.
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param long0		longitude of origin (in radians)
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param units		projection unit
	*/
	TeSinusoidal (const TeDatum& datum,  
		         double long0, 
				 double offx = 0., 
				 double offy = 0.,
				 const string& units = "Meters"): 
	TeProjection ( "Sinusoidal", datum, long0, 0., offx, offy, 0., 0., units )
	{}

//! Empty destructor.
	~TeSinusoidal () {}

	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual TeCoord2D PC2LL(TeCoord2D& p);
};

//! Provides methods that are required to handle the Cylindrical Equidistant map projection
class TL_DLL TeCylindricalEquidistant : public TeProjection
{
public:

//!	Constructor.
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param lon0			longitude of origin (in radians)
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param stlat1		first standard parallel (in radians)
		\param units		projection unit
	*/
	TeCylindricalEquidistant (const TeDatum& datum,  
		         double lon0, 
				 double offx = 0., 
				 double offy = 0.,
				 double stlat1 = 0.,
				 const string& units = "Meters"):
			TeProjection ( "CylindricalEquidistant", datum, lon0, 0., offx, offy, stlat1, 0,units)
		{}
//! Empty destructor.
	~TeCylindricalEquidistant () {}

	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual TeCoord2D PC2LL(TeCoord2D& p);
};

//! Provides methods that are required to handle the Polar Stereographic map projection
class TL_DLL TePolarStereographic : public TeProjection
{
public:
	//! Constructor
	/*!	
		Initializes projection parameters 
		\param datum		spheroid
		\param lon0			longitude of origin (in radians)
		\param offx			X (projection coordinate) offset (m)
		\param offy 		Y (projection coordinate) offset (m)
		\param units		projection unit
		\param hem	hemisphere
	*/
	TePolarStereographic ( const TeDatum& datum,  
		         double lon0,  
				 double offx = 0., 
				 double offy = 0.,
				 const string& units = "Meters",
				 const TeHemisphere hem = TeSOUTH_HEM ):
	TeProjection ( "PolarStereographic", datum, lon0, 0., offx, offy, 0, 0, units, 1, hem )
	{}

	//! Destructor
	~TePolarStereographic () {}

	virtual TeCoord2D LL2PC(TeCoord2D& p);
	virtual	void	LL2PC (ostream&) const; //FAMI
	virtual TeCoord2D PC2LL(TeCoord2D& p);

	virtual void LL2PC(double xi, double yi, double& xo, double& yo) const; //FAMI
};

//FAMI
class TL_DLL TeSatelliteProjection : public TeProjection
{

private:
	
	double	SPri,	// Sensor angle resolution along y axis in radians
		SPrj,	// Sensor angle resolution along x axis in radians
		SPis,	// Y-coordinate of sub-satellite point 
		SPjs,	// X-coordinate of sub-satellite point
	        SPrs,   // Radius of satellite orbit in meters
		SPscn,	// Scanning mode: 0-WE/NS, 1-SN/EW
		SPyaw;  // Grid orientation, i.e., angle in radians between
			// the increasing y axis and the meridian of the
			// sub-satellite point along the direction of
			// increasing latitude.
public:

	//! Constructor
	/*
		\param datum: 	planimetric datum
		\param offx: 	x offset
		\param offy: 	y offset
		\param Pri:     Sensor angle resolution along y axis in radians
		\param Prj: 	Sensor angle resolution along x axis in radians
		\param Pis:     Y-coordinate of sub-satellite point 
		\param Pjs: 	X-coordinate of sub-satellite point
		\param Pla0: 	Latitude of sub-satellite point in radians
		\param Plo0: 	Longitude of sub-satellite point in radians
		\param Prs: 	Radius of satellite orbit in meters
		\param Pscn: 	Scanning mode: 0-WE/NS, 1-SN/EW
		\param Pyaw: 	Grid orientation, i.e., angle in radians between
				the increasing y axis and the meridian of the
				sub-satellite point along the direction of
				increasing latitude.
		*/
	TeSatelliteProjection(const TeDatum& datum, double offx, double offy,  
			      double Pri, double Prj, double Pis, double Pjs, double Pla0, double Plon0, 
			      double Prs,double Pscn, double Pyaw):
			TeProjection("Satellite", datum, Plon0, Pla0, offx, offy, 0., 0.,"Meters",1.,TeSOUTH_HEM),
			SPri(Pri),
			SPrj(Prj),
			SPis(Pis),
			SPjs(Pjs),		
			SPrs(Prs),
			SPscn(Pscn),
			SPyaw(Pyaw)	{}

	~TeSatelliteProjection() {}

	virtual TeCoord2D LL2PC(TeCoord2D& p);

	virtual	TeCoord2D PC2LL(TeCoord2D& p);

	// Returns a the parameters of this projection
	virtual TeProjectionParams params () const;

	double RadiusSatOrbit () { return SPrs; }
};

class TL_DLL TeNoProjection : public TeProjection
{
public:
	TeNoProjection(const TeDatum& datum = TeDatum(), const string& units = "Units"):
		TeProjection ( "NoProjection", datum, 0., 0., 0., 0., 0, 0, units, 1, TeSOUTH_HEM )

	{	GPname = "NoProjection", GPunits = units; }

		~TeNoProjection () {}

		virtual TeCoord2D LL2PC(TeCoord2D& p) { return p; }

		virtual TeCoord2D PC2LL(TeCoord2D& p) {return p;};
};

bool TL_DLL decodifyDescription(const string& projDescription, TeProjectionParams& pars);

//! Creates a TeProjection instance from a PROJ4 description
/*
	\note This function works only for sproj descriptionsgenerated by TerraLib
*/
TL_DLL TeProjection* TeGetTeProjectionFromSProj(const string& sproj4desc);

//! Generates a PROJ4 description from a TerraLib instance
TL_DLL string TeGetSProjFromTeProjection(TeProjection* teproj);

//! Generates OGC WKT Spatial Reference description from a TerraLib instance
TL_DLL string TeGetWKTFromTeProjection(TeProjection* proj);

//! Creates a TeProjection instance from a OGC WKT Spatial Reference description
/*
	\note This function works only for sproj descriptions generated by TerraLib
*/
TL_DLL TeProjection* TeGetTeProjectionFromWKT(const string& wkt);

/** \example convertCoordinates.cpp
 * This is an example of how to convert a coordinate from a projection to another
 */


#endif


