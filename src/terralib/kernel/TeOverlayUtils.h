/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/**
 * @file TeOverlayUtils.h
 * @brief This file contains support algorithms for set operations.
 * These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone.
 * THIS IS FOR INTERNAL USE ONLY.
 */

/** @ingroup SetOperations
    THIS IS FOR INTERNAL USE ONLY: TerraLib set operation auxiliary functions. 
 *  @{
 */

#ifndef  __TERRALIB_INTERNAL_OVERLAYUTILS_H
#define  __TERRALIB_INTERNAL_OVERLAYUTILS_H

// STL's include
#include <map>
#include <vector>
#include <algorithm>
using namespace std;

#include "TeGeometry.h"
#include "TeIntersector.h"


namespace TeOVERLAY
{

//---------------- Auxiliary structures ----------------//
//! Defines a functor for coordinate order during map insert and retrival: lexicograpgical order (xy)
struct TL_DLL xyOrder
{
	//! Default operation for 'less than' tests.
	bool operator()(const TeCoord2D& c1, const TeCoord2D& c2) const
	{
		if(c1.x_ < c2.x_)
			return true;

		if(c1.x_ > c2.x_)
			return false;

		if(c1.y_ < c2.y_)
			return true;		

		return false;
	}
};

//! Type to index fragments end points: used during merge fase.
typedef multimap<TeCoord2D, pair<unsigned int, TeLine2D>, xyOrder> TeLineIndex;

//! Defines a functor for ordering segments during point in poly tests.
struct TL_DLL segOrder
{
	//! Default operation for 'less than' tests.
	bool operator()(const TeINTERSECTOR2::TeSegIdInPolygonSet& ip1, const TeINTERSECTOR2::TeSegIdInPolygonSet& ip2) const
	{
		if(ip1.polId_ < ip2.polId_)
			return true;		

		return false;
	}
};
//---------------- Auxiliary operations for overlay ----------------//

//! Verifies orientation for each line of polygon set, and reverse the orientation if need
TL_DLL void TeFixOrientation(TePolygonSet& polSet, const short& outerOrientationToReverse, const short& innerOrientationToReverse);


//! For each operation (union, intersection and difference) defines location for retrieval of fragments
TL_DLL void TeChooseBoundaryLocation(const short& operation, short& locationRedFragments, short& locationBlueFragments);


//! Erases from fragmentsIndex boundary fragments that are in oposite direction: each fragment must have a unique identifier in the pair first field (pair<unsigned int, TeLine2D>)
TL_DLL void TeRemoveOpositeBoundaryFragments(TeLineIndex& fragmentsIndex);


//! Erases from fragmentsIndex boundary fragments that are equals to another boundary fragment
TL_DLL void TeRemoveSameBoundaryFragments(TeLineIndex& fragmentsIndex);

//! Merge fragments ito first index (fragmentsIndex)
TL_DLL void TeJoinFragments(TeLineIndex& fragmentsIndex, TeLineIndex& boundaryFragmentsIndex);

//! Moves closed rings from fragmentsIndex to rins vector
TL_DLL void TeFindAndMoveClosedRings(TeLineIndex& fragmentsIndex, vector<TeLinearRing>& rings);

//! Gets a polygonset with outer rings and a vector with holes and try to find to what polygon the hole belongs to
TL_DLL bool TeMountTopology(TePolygonSet& polysOut, vector<TeLinearRing>& holes);

//! Make polygons from fragments.
TL_DLL bool TeMergeFragments(TeLineIndex& fragmentsIndex, vector<TeLinearRing>& rings, const bool& doExaustive = false);

//! For each linear ring, see it's orientation and classify in outer or inner ring
TL_DLL bool TeClassifyRings(vector<TeLinearRing>& rings, TePolygonSet& polsOut, vector<TeLinearRing>& holes);

//! Find fragments in the red set that satisfies locationFragments using blue set as reference
TL_DLL void TeRtreeGetFragments(const TePolygonSet& bluePolygons, TeINTERSECTOR2::TeSegmentRTree& blueTree, TeLineSet& redFragments, const short& locationFragments, short& mask, TeLineIndex& redFragmentsIndex, vector<TeLinearRing>& rings);

//! Find fragments in the same set that satisfies locationFragments
TL_DLL void TeRtreeGetFragments(const TePolygonSet& polygons, TeINTERSECTOR2::TeSegmentRTree& tree, TeLineSet& fragments, vector<pair<unsigned int, unsigned int> >& fragmentsIds, const short& locationFragments, short& mask, TeLineIndex& fragmentsIndex, vector<TeLinearRing>& rings);

//! Removes fragments that may overlap with others: used in special cases during union process
TL_DLL void TeRtreeRemoveFragments(const TePolygonSet& polygons, TeINTERSECTOR2::TeSegmentRTree& tree, TeLineIndex &lineIndex, vector<pair<unsigned int, unsigned int> >& fragmentsIds, const short& locationFragments, short& mask, vector<TeLinearRing>& rings);

//! Split rings if they have commom end points.
/*
	\param  ring		Ring to be broken.
	\param  ringsOut	Resulting rings.
	\return				Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeSplitRing(TeLinearRing& ring, TeLineSet& ringsOut);

//! Split rings if they have commom end points.
/*
	\param  rings		A vector of linear rings to be broken.
	\param  ringsOut	Resulting rings.
	\return				Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeSplitRings(vector<TeLinearRing>& rings, vector<TeLinearRing>& ringsOut);

//! Clone line point removing duplicated coordinates.
/*
	\param lineIn   Line to ble cloned.
	\param lineOut  Cloned line without repeated points.  
	\param minPts	This is a constant to check if the cloned line has the minimum number of points.
	\return         Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeCloneLine(const TeLine2D& lineIn, TeLine2D& lineOut, const unsigned int& minPts);

//! Clone polygon lines and try to remove duplicated coordinates from lines.
/*
	\param polIn       Polygon to be cloned.
	\param polOut      Cloned polygon without repeated points.
	\return            Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeClonePolygon(const TePolygon& polIn, TePolygon& polOut);

//! Clone polygons lines and try to remove duplicated coordinates from lines.
/*
	\param polsIn       Polygons to be cloned.
	\param polsOut      Cloned polygons without repeated points.
	\return             Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeClonePolygonSet(const TePolygonSet& polsIn, TePolygonSet& polsOut);

}	// end namespace TeOVERLAY

#endif	// __TERRALIB_INTERNAL_OVERLAYUTILS_H


