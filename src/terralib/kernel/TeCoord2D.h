/*
 * (C) Copyright 1996-2016 ECMWF & INPE.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TeCoord2D.h
    \brief This file contains the definition of a classes to handle 2D coordinates
*/
#ifndef  __TERRALIB_INTERNAL_COORD2D_H
#define  __TERRALIB_INTERNAL_COORD2D_H

#include <math.h>
#include "TeDefines.h"
#include "TePrecision.h"

#include <vector>

using namespace std;

//! A class for handling 2D coordinates
class TL_DLL TeCoord2D
{
public:

	double	x_;		//!< x coordinate
	double	y_;		//!< y coordinate

	//! Default contructor
	TeCoord2D ( const double& x = 0., const double& y = 0. ):
		x_ ( x ), y_ ( y ) {}

	//! Copy constructor
	TeCoord2D (const TeCoord2D& cd) 			
		{x_ = cd.x_; y_ = cd.y_;}

	//! Operator =
	TeCoord2D& operator= ( const TeCoord2D& cd )
	{
		if ( this != &cd )
		{	
			x_ = cd.x_; y_ = cd.y_;
		}
		return *this;
	}
		
	//!	Empty destructor.
	~TeCoord2D () { }

	//!	Returns the X componente of the coordinate
	const double&	x () const 				
	{	return x_;	}

	//!	Sets the X componente of the coordinate
	void x ( const double& valX )
	{	x_ = valX; }

	//!	Returns the Y componente of the coordinate
	const double& 	y () 		const		
	{	return y_;	}

	//!	Sets the Y componente of the coordinate
	void y(const double& valY)
	{	y_ = valY; }

	//!	Sets the X and Y componentes of the coordinate
	void setXY(const double& xVal, const double& yVal )
	{	x_= xVal; y_= yVal; }

	//!	Adds two coordinates
	void 	operator+=(const TeCoord2D& cd)	{x_ += cd.x_; y_ += cd.y_;}

	//!	Returns TRUE if current coordinate is smaller than or equal	to TeCoord2D cd; returns FALSE otherwise 
	bool  operator<= (const TeCoord2D& cd) const 
		{return (( y_ - cd.y_ <= TePrecision::instance().precision() ) && 
		         ( x_ - cd.x_ <= TePrecision::instance().precision() ) );}	

	//!	Returns TRUE if current coordinate is equal	to TeCoord2D cd; returns FALSE otherwise 
	bool operator== (const TeCoord2D& cd) const 
	{return (  ( fabs (y_ - cd.y_) < TePrecision::instance().precision() ) 
			&& ( fabs (x_ - cd.x_) < TePrecision::instance().precision() ) ); }

	//!	Returns TRUE if current coordinate is not equal to TeCoord2D cd; returns FALSE otherwise 
	bool	operator!= (const TeCoord2D& cd) const 
		{return ( ( fabs (y_ - cd.y_) >= TePrecision::instance().precision() ) 
		       || ( fabs (x_ - cd.x_) >= TePrecision::instance().precision() ) );}	

	//!	Returns TRUE if current coordinate is smaller than TeCoord2D cd; returns FALSE otherwise 
	bool	operator< (const TeCoord2D& cd) const 
		{return ( y_ - cd.y_ < TePrecision::instance().precision() 
		       && x_ - cd.x_ < TePrecision::instance().precision() );}	

	//!	Returns TRUE if current coordinate greater than maximum allowed; returns FALSE otherwise 
	bool	tooBig ()
		{return ( y_ > TeMAXFLOAT/10.|| x_ > TeMAXFLOAT/10.);}	

	//!	Rescales the coordinate by a value in X (xscale) and in Y (yscale)
	void	scale ( const double xscale, const double yscale )
	{ x_ = x_* xscale; y_ = y_ * yscale; }


	//!	Returns the middle coordinate in horizontal direction between current coordinate and rhs
	TeCoord2D halfWayHoriz ( TeCoord2D& rhs )
	{
		TeCoord2D mid (( rhs.x() + x() ) / 2., y() );
		return mid;
	}

	//!	Returns the middle coordinate in vertical direction between current coordinate and rhs
	TeCoord2D halfWayVert  ( TeCoord2D& rhs )
	{
		TeCoord2D mid ( x(), ( y() + rhs.y() ) / 2. );
		return mid;
	}

	//! A false interface to set an obejct identifier
	void objectId(const string& /* id */ ) 
	{	return;  }
};

//! An structure that contains a pair of coordinates
struct TL_DLL TeCoordPair
{
	TeCoord2D pt1;	//!< First coordinate value.
	TeCoord2D pt2;	//!< Second coordinate value.

	//! Default contructor: [(0.0),(0.0)]
	TeCoordPair():
		pt1(TeCoord2D(0.0,0.0)),
		pt2(TeCoord2D(0.0,0.0))
	{}

	//! Constructor with parameters
	TeCoordPair(const TeCoord2D& p1, const TeCoord2D& p2):
		pt1(p1),
		pt2(p2)
		{}
};

//! A vector of pairs of coordinates
typedef vector<TeCoordPair> TeCoordPairVect;

#endif

