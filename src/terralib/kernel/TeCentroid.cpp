/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

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

#include <vector>
#include <functional>
#include <algorithm>

#include "TeAssertions.h"
#include "TeException.h"
#include "TeGeometryAlgorithms.h"

using namespace std;


// Prototypes for internal routines - NOT seen from the outside

TeCoord2D TeFindBaricenter (const TeLinearRing& ring ); // general method ( solves most cases)
TeCoord2D TeFindCentroidConcavePolygon (const TePolygon& poly ); // brute-force approach (use if above fails)

TeCoord2D
TeFindCentroid(TeMultiGeometry& mGeom )
{
	TeBox bb;
	TeCoord2D ct;
	if(mGeom.hasPolygons())
	{
		ct = TeFindCentroid(mGeom.getPolygons());
		updateBox(bb,ct);
	}
	if(mGeom.hasCells())
	{
		ct = TeFindCentroid(mGeom.getCells());
		updateBox(bb,ct);
	}
	if(mGeom.hasLines())
	{
		ct = TeFindCentroid(mGeom.getLines());
		updateBox(bb,ct);
	}
	else if(mGeom.hasPoints())
	{
		ct = TeFindCentroid(mGeom.getPoints());
		updateBox(bb,ct);
	}
	return bb.center();
}

TeCoord2D
TeFindCentroid(const TePolygon& poly )
{
	// This is a two-step algorithm

	// First, try a simple geometric solution
	// that works for convex polygons and some concave ones

	TeCoord2D pt = TeFindBaricenter( poly[0] );

	if ( TeWithin ( TePoint(pt), poly) )
		return pt;
	
	// No, try a more general approach

	pt = TeFindCentroidConcavePolygon( poly );

return pt;
}

TeCoord2D
TeFindCentroid (const TeLine2D& line )
{
	int midpoint = (line.size()/2) - 1 ;
	TeCoord2D middle;
	
	TeGetMiddlePoint(line[midpoint], line[midpoint+1], middle);
	return middle;
}

TeCoord2D 
TeFindCentroid(const TeCell& c) 
{
	return c.box().center();
}

TeCoord2D 
TeFindCentroid(const TePoint& p) 
{
	return p.location();
}

TeCoord2D 
TeFindCentroid(const TeText& t) 
{
	return t.location();
}

TeCoord2D 
TeFindCentroid(const TePolygonSet& s)
{
	TeCoord2D p;
	if (s.empty())
		return p;
	double sx = 0.0;
	double sy = 0.0;
	unsigned int n = 0;

	TeBox bb = s.box();
	TePolygonSet::iterator it1 = s.begin(); 
	while (it1 != s.end())
	{
		TeCoord2D p1 = TeFindCentroid (*it1);
		sx += p1.x();
		sy += p1.y();
		++n;
		++it1;
	}
	double ssx = sx/n;
	double ssy = sy/n;
	p.setXY(ssx,ssy);
	return p;
}


TeCoord2D 
TeFindCentroid(const TeLineSet& s)
{
	TeCoord2D p;
	if (s.empty())
		return p;
	double sx = 0.0;
	double sy = 0.0;
	unsigned int n = 0;
	TeLineSet::iterator it1 = s.begin(); 
	while (it1 != s.end())
	{
		TeCoord2D p1 = TeFindCentroid (*it1);
		sx += p1.x();
		sy += p1.y();
		++n;
		it1++;
	}
	sx = sx/n;
	sy = sy/n;
	p.setXY(sx,sy);
	return p;
}


TeCoord2D 
TeFindCentroid(const TeCellSet& s)
{
	TeCoord2D p;
	if (s.empty())
		return p;
	double sx = 0.0;
	double sy = 0.0;
	unsigned int n = 0;
	TeCellSet::iterator it1 = s.begin(); 
	while (it1 != s.end())
	{
		TeCoord2D p1 = TeFindCentroid (*it1);
		sx += p1.x();
		sy += p1.y();
		++n;
		it1++;
	}
	sx = sx/n;
	sy = sy/n;
	p.setXY(sx,sy);
	return p;
}

TeCoord2D 
TeFindCentroid(TePointSet& points)
{
	TeCoord2D p;
	if (points.empty())
		return p;
	if (points.size() == 1)
		p = (points[0]).location();
	else
		p = points.box().center();
	return p;
}

TeCoord2D 
TeFindCentroid(TeTextSet& texts)
{
	return texts.box().center();
}
/*
 * ANSI C++ code from the article
 * "Centroid of a Polygon"
 * by Gerard Bashein and Paul R. Detmer,
 * in "Graphics Gems IV", Academic Press, 1994
 */

/*********************************************************************
Centroid: Calculates the centroid (xCentroid, yCentroid) 
of a polygon, given its vertices (x[0], y[0]) ... (x[n-1], y[n-1]). It
is assumed that the contour is closed, i.e., that the vertex following
(x[n-1], y[n-1]) is (x[0], y[0]).  

**********************************************************************/

TeCoord2D
TeFindBaricenter ( const TeLinearRing& ring )
{

    double ai, atmp = 0, xtmp = 0, ytmp = 0;
         
    int n = ring.size() - 1; // our polygons wrap around
	int i, j;

    if (n < 3) return TeCoord2D (0., 0.);

	for ( i = n-1, j = 0; j < n; i = j, j++)
 	{
 	  	ai = ring[i].x() * ring[j].y() - ring[j].x() * ring[i].y();
	  	atmp += ai;
	  	xtmp += (ring[j].x() + ring[i].x()) * ai;
	  	ytmp += (ring[j].y() + ring[i].y()) * ai;
	}

    if (atmp != 0)
	{
		return TeCoord2D ( (xtmp / (3. * atmp)), ( ytmp / (3. * atmp)));
	}
	return TeCoord2D ( 0., 0. );
}



//
//	TeFindCentroidConcavePolygon 
//
//  This algorithm is based on the idea of a plane-sweep
//
//  1. Divide the box into horizontal slices
//
//  2. Sweep the plane, starting from the bottom
//
//  3. For each y-slice, find the horizontal intersections in the 
//     forward direction (if it exists)
//
//  4. Sort these intersections
//
//  5. Order the intersections 
//
//  6. For all possible "intersections", select a mid-point and
//     test if this point is inside the polygon
//
//  7. If a centroid has not been found, cut the box into
//     "smaller" slices and repeat steps 2-6



// Defines a function object for sorting coordinates in ascending x-order
 
struct x_order : public binary_function<TeCoord2D, TeCoord2D, bool>
{
        bool operator()(TeCoord2D& pt1, TeCoord2D& pt2) { return pt1.x() < pt2.x(); }
};

// Defines a distance measure between two Coordinate pairs (used for sorting)

struct x_dist : public binary_function<TeCoordPair, TeCoordPair, bool>
{
		bool operator()(TeCoordPair c1, TeCoordPair c2)
		{
			return ( ( c1.pt2.x() - c1.pt1.x() ) > ( c2.pt2.x() - c2.pt1.x() ) ); 
		}
};


typedef vector<double> doubleVect;

TeCoord2D
TeFindCentroidConcavePolygon ( const TePolygon& poly )
{

	TeLinearRing coords = poly[0];

	TeBox box = coords.box();

	int NSLICES = 10;

	int NMAXSLICES = 400;

	double yinit = box.lowerLeft().y();
		
	while ( true )
	{
		int nparts = NSLICES + 2; // divide the box into slices

		double increm = ( box.upperRight().y() - box.lowerLeft().y() ) / nparts; // y-increment

		double yslice = yinit; // start at the bottom

		TeCoordPairVect intersecList;  //vector of horizontal intersections

		doubleVect crossList; //vector of x-crossings

		for ( int i = 0; i < NSLICES; i++ )
		{
			yslice += increm; //next slice

			for (unsigned int x = 0; x < ( coords.size() - 1 ); x++ )
				TeCoord2D cd= coords[x];

			for  (unsigned int j = 0;  j < ( coords.size() - 1 ); j++ ) 		
			{

				TeCoord2D vtx0 = coords[j];
				TeCoord2D vtx1 = coords[j+1];

				bool yflag0 = ( vtx0.y() >= yslice );
				bool yflag1 = ( vtx1.y() >= yslice ) ;
				/* check if endpoints straddle (are on opposite sides) of X axis
				* (i.e. the Y's differ); if so, +X ray could intersect this edge.
				*/

				if ( yflag0 != yflag1 )
				{
					// line crosses ring horizontally 

					double slope =  ( vtx1.x() - vtx0.x() ) / ( vtx1.y() - vtx0.y());
					double xcross = ( yslice -   vtx0.y() )* slope + vtx0.x();
			
					crossList.push_back ( xcross );	
				}

			}

			// Sort the x-intersections

			sort ( crossList.begin(), crossList.end() );

			doubleVect::iterator it = crossList.begin();
			
			// Create a list of intersection points

			while ( it != crossList.end() )
			{
				TeCoordPair cp;

				cp.pt1 = TeCoord2D ( (*it), yslice );
				++it;
				if ( it == crossList.end() ) break;

				cp.pt2 = TeCoord2D ( (*it), yslice );
				++it;

				intersecList.push_back ( cp );
			}
					
		} // for all slices

		// Now sort all the intersections

		sort ( intersecList.begin(), intersecList.end(), x_dist() );

		// Go through the intersection list
		// Find the mid-point

		TeCoordPairVect::iterator it = intersecList.begin();
		
		while ( it != intersecList.end() )
		{
			double xmid = (*it).pt1.x() + ( (*it).pt2.x() - (*it).pt1.x() ) / 2;

			TeCoord2D pt ( xmid, (*it).pt2.y() );


			if ( TeWithin (TePoint(pt), poly) ) // have we found it ???
				return pt;

			++it;  // try the next largest x-intersection
		}

		NSLICES *= 2;  // double the number of slices

		// iterate until a maximum number of slices
		// if not able to find a centroid, log an error, but don't stop
		if ( NSLICES > NMAXSLICES ) 
		{
			string userText = "Polygon number " + poly.geomId ();

			TeErrorLog::instance().insert ( CENTROID_NOT_FOUND, userText );

			return TeCoord2D ( TeMAXFLOAT, TeMAXFLOAT );
		}
	}

}


