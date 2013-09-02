/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2004 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/

#include "TeGeometry.h"
#include "TeException.h"

// TeLine2D

bool
TeLine2D::isRing() const
{
	if ( size() <= 3 )
			return false;
		return ( pImpl_->operator[] (0) == pImpl_->operator[] ( pImpl_->size() - 1 ) );

}


// TeLinearRing

TeLinearRing::TeLinearRing ( TeLine2D& line ): TeLine2D ( line ) 
{
	if ( ! line.isRing() )
	{
		line.add (line[0]);
//		throw TeException ( LINE_IS_NOT_RING );
	}
}


ostream& operator<<(ostream& os, TeNode& N)
{
	os << Te2String(N.geomId());
    return os;
}

ostream& operator<<(ostream& os, const TeArc& N)
{
	TeArc a = N;
	os << a.objectId();
    return os;
}

void 
TeLineSet::copyElements ( const TeLineSet& other )
{
	geomId_ = other.geomId();
	objectId_ = other.objectId();

	for (unsigned int i=0; i<other.size(); ++i) // for each line
	{
		TeLine2D line; 
		line.copyElements(other[i]);
		this->add (line);
	}
}

void 
TePolygon::copyElements ( const TePolygon& other )
{
	geomId_ = other.geomId();
	objectId_ = other.objectId();

	for (unsigned int i = 0; i < other.size(); ++i) // for each linear ring
	{
		TeLine2D line; 
		line.copyElements(other[i]);
		this->add (line);
	}
}


void 
TePolygonSet::copyElements ( const TePolygonSet& other )
{
	geomId_ = other.geomId();
	objectId_ = other.objectId();

	for (unsigned int i = 0; i < other.size(); ++i) // for each polygon
	{
		TePolygon poly;
		poly.copyElements (other[i]);
		this->add (poly);
	}
}


TePointSet makePointSet( const TeLinearRing& lr )
{
  TePointSet outps;
  TePoint temp_point;
  
  const unsigned int lr_size = lr.size();
  
  for( unsigned int lr_index = 0 ; lr_index < lr_size ; ++lr_index ) {
    TePoint temp_point;
    temp_point.add( lr[ lr_index ] );
    
    outps.add( temp_point );
  }
  
  return outps;
}


TePointSet makePointSet( const TePolygon& p )
{
  TePointSet outps;

  const unsigned int p_size = p.size();
  unsigned p_index = 0;
  unsigned int lrps_size = 0;
  unsigned int lrps_index = 0;
  
  for( p_index = 0 ; p_index < p_size ; ++p_index ) {
    TePointSet lrps = makePointSet( p[ p_index ] );
    lrps_size = lrps.size();
  
    for( lrps_index = 0 ; lrps_index < lrps_size ; ++lrps_index ) {
      outps.add( lrps[ lrps_index ] );
    }
  }
  
  return outps;
}

