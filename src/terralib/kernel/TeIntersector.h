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
/*! \file  TeIntersector.h
    \brief This file contains structures and definitions for line intersection algorithms.
	\note  These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone because
           THIS IS FOR INTERNAL USE ONLY.
    \author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
*/

#ifndef  __TERRALIB_INTERNAL_INTERSECTOR2_H
#define  __TERRALIB_INTERNAL_INTERSECTOR2_H

//TerraLib's include
#include "TeCoord2D.h"
#include "TeRTree.h"

//STL's include
#include <vector>

using namespace std;

/* 
 *  WARNING: These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone because
 *           the support and interfaces can be changed in future. THIS IS FOR INTERNAL USE ONLY.
 */

/*! \brief Contains structures and definitions needed by line intersection algorithms (FOR INTERNAL USE ONLY).
*/
namespace TeINTERSECTOR2
{
//! An epsilon value used to compare two real numbers
#define EPSILON_COMPARE 1.0e-15

/** @defgroup IntersectionAlgorithms Intersection Algorithms
 *  Intersection Algorithms and data structures, used internally.
 *  @{
 */

//! This struct is used to represent a point intersection between two segments on boundary of a TePolygon or TeLine2D.
struct TL_DLL TeBoundaryIP
{
	vector<TeCoord2D> coords_;		//!< Points of intersection ocurried along these two segments (red and blue).

	unsigned int redSegNum_;		//!< Red segment number.
	unsigned int redPartNum_;		//!< Line number in a polygon that a red segment belongs.
	unsigned int redPolNum_;		//!< Polygon number in a vector of polygons that a segment belongs.

	unsigned int blueSegNum_;		//!< Blue segment number.
	unsigned int bluePartNum_;		//!< Line number in a polygon that a blue segment belongs.
	unsigned int bluePolNum_;		//!< Polygon number in a vector of polygons that a segment belongs.
};

//! This is the type of intersection point list.
typedef vector<TeBoundaryIP> TeVectorBoundaryIP;

//! This struct represents an index to the right place of a segment in a TeLineSet, TeLine2D, TePolygon or TePolygonSet.
struct TL_DLL TeSegIdInPolygonSet
{
	unsigned int polId_;	//!<  The polygon id, when used in a polygonset.
	unsigned int lineId_;	//!<  The line id, when used in a lineset or in a polygon.
	unsigned int segId_;	//!<  The segment id into a specified line.
};

//! This pair is used to index two segments that intersects.
typedef pair<TeSegIdInPolygonSet, TeSegIdInPolygonSet> TePairSegIdInPolygonSet;

//! This is the type used to index the segments in the boundary of a TeLine2D, TeLineSet, TePolygon or TePolygonSet.
typedef TeSAM::TeRTree<TeSegIdInPolygonSet, 8> TeSegmentRTree;

/** \brief Tells if three points makes a right turn, a left turn or are collinear.
    \param c1       The first coordinate.
	\param c2       The second coordinate.
	\param c3       The coordinate to test the relative position.
	\param between  Tells if c3 is between c1 and c2.
	\return			The orientation: TeCLOCKWISE, TeCOUNTERCLOCKWISE or TeNOTURN.
*/
TL_DLL short TeCCW(const TeCoord2D& c1, const TeCoord2D& c2, const TeCoord2D& c3, bool& between);

/** \brief Returns the intersection point of the segments.
    \param a                 The first coordinate of the first segment.
	\param b                 The second coordinate of the first segment.
	\param c                 The first coordinate of the second segment.
	\param d                 The second coordinate of the second segment.
	\param ips               The intersection coordinates (0, 1 or 2).
	\param intersectionType  An intersection may be proper or improper.
	\return					 Returns true if there is an intersection between segments defined by end coordinates.
*/
TL_DLL bool TeIntersection(const TeCoord2D& a, const TeCoord2D& b, const TeCoord2D& c, const TeCoord2D& d, TeBoundaryIP& ips, TeSegmentIntersectionType& intersectionType);


/** \brief Tells if two segments intersects.
    \param a                 The first coordinate of the first segment.
	\param b                 The second coordinate of the first segment.
	\param c                 The first coordinate of the second segment.
	\param d                 The second coordinate of the second segment.
	\param intersectionType  An intersection may be proper or improper.
	\return					 Returns true if there is an intersection between segments defined by end coordinates.
*/
TL_DLL bool TeIntersects(const TeCoord2D& a, const TeCoord2D& b, const TeCoord2D& c, const TeCoord2D& d, TeSegmentIntersectionType& intersectionType);


/** \brief Verifies if there is an intersection between two given polygonsets.
    \param redPols  The first polygonset to test.
	\param bluePols The second polygonset to test.
	\param report   A list with the intersection points.
	\return			Returns true if there is an intersection between segments of the polygons.
	
	\note WARNING: this is deprecated and will be replaced by another function in near future.

    This is a lazy algorithm. It can be used, for example, in intersections between a box and a line.
	It is O(n*m) - where n and m are the numbers of segments in the first and second polygonsets.
 */
TL_DLL bool TeSafeIntersections(const TePolygonSet& redPols, const TePolygonSet& bluePols, TeVectorBoundaryIP& report);

/** \brief Verifies if there is an intersection between two given lines.
    \param redLine   The first line to test.
	\param blueLine  The second line to test.
	\param report    A list with the intersection points.
	\param redObjId  Red line object id.
	\param blueObjId Blue line object id.
	\return			 Returns true if there is an intersection between segments of the lines.
	
	\note WARNING: this is deprecated and will be replaced by another function in near future.

    This is a lazy algorithm. It can be used, for example, in intersections between a box and a line.
	It is O(n*m) - where n and m are the numbers of segments in the first and second lines.
 */
TL_DLL bool TeSafeIntersections(const TeLine2D& redLine, const TeLine2D& blueLine, TeVectorBoundaryIP& report, const unsigned int& redObjId = 0, const unsigned int& blueObjId = 0);

/** \brief Returns true if the lines intersects.
	\param redLine			The line to test.
	\param blueLine			The line to test.
	\return			        Returns true if there is an intersection between segments of the lines.

	\note WARNING: this is deprecated and will be replaced by another function in near future.
 */
TL_DLL bool TeIntersects(const TeLine2D& redLine, const TeLine2D& blueLine);

/**	\brief Reports intersections between segments of polygons in the polygonset list.
	\param polygons		A list of polygons to test self intersections.
	\param tree			The tree with all index segments, it WILL BE filled inside this method.
	\param report		A report list of intersection points.
	\return				Returns true if there is an intersection.

	\note				This function will not returns intersections between segments of the same polygon. It will only
	                    report intersection between different polygons. The index tree MUST BE EMPTY, othewise, the
						result is undefined. The result may have duplicated points because intersections are reported 
						twice: this will turn fragmentation easier.
 */
TL_DLL bool TeIntersection(const TePolygonSet& polygons, TeSegmentRTree& tree, TeVectorBoundaryIP& report);

/** \brief Reports intersections between segments of two diferent polygonsets.
	\param redPolygons	A list of polygons without self intersections.
	\param redTree		A tree with all red segment already indexed or not, if it is empty, so, it will be filled inside this method.
	\param bluePolygons	A list of polygons without self intersections.
	\param report		A report list of intersection points.
	\return				Returns true if there is an intersection between a red segment and a blue segment.

	\note				This function will not returns intersections between segments of the same polygon. It will only
	                    report intersection between different polygons. The result may have duplicated points because intersections are reported 
						twice: this will turn fragmentation easier.
 */
TL_DLL bool TeIntersection(const TePolygonSet& redPolygons, TeSegmentRTree& redTree, const TePolygonSet& bluePolygons, TeINTERSECTOR2::TeVectorBoundaryIP& report);

/** \brief Reports intersections between segments of a lineset and a polygonset.
	\param redLines	    A list of lines without self intersections.
	\param bluePolygons	A list of polygons without self intersections.
	\param blueTree		A tree with all segments from the polygonset already indexed or not, if it is empty, so, it will be filled inside this method.
	\param report		A report list of intersection points.
	\return				Returns true if there is an intersection between a red segment from the lineset and a blue segment from the polygonset.

	\note				This function will not returns intersections between segments of the same polygon or lineset. It will only
	                    report intersection between segments from different sets.
 */
TL_DLL bool TeIntersection(const TeLineSet& redLines, const TePolygonSet& bluePolygons, TeSegmentRTree& blueTree, TeINTERSECTOR2::TeVectorBoundaryIP& report);

/** \brief Tells if there is a self-intersection between segments.
	\param polygons					A list of polygons to test self intersections.
	\param selfIntersectionList		A report list with all self-intersections.
	\return							Returns true if there is not a self-intersection.
 */
TL_DLL bool TeIsSimple(const TePolygonSet& polygons, vector<TePairSegIdInPolygonSet>& selfIntersectionList);

/** \brief Index polygon's segments.
	\param polygons		A list of polygons to index it segment's.
	\param tree			The tree if indexed segments.
 */
TL_DLL void TeIndexPolygonSet(const TePolygonSet& polygons, TeSegmentRTree& tree);

/** @} */ 

}	// end namespace TeINTERSECTOR2
#endif //__TERRALIB_INTERNAL_INTERSECTOR2_H

