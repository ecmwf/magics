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
/*! \file TeGeometryAlgorithms.h
    \brief This file contains Algorithms for Topological Operations.
	\author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
*/
#ifndef  __TERRALIB_INTERNAL_GEOMETRYALGORITHMS_H
#define  __TERRALIB_INTERNAL_GEOMETRYALGORITHMS_H

#include "TeGeometry.h"
#include "TeMultiGeometry.h"

#include "TePrecision.h"
#include "TeProjection.h"

//! Intersection coordinates of two segments.
typedef vector<TeCoord2D> TeIntersCoordsVec;

/** @defgroup GeometryAlgorithms Geometry algorithms
	TerraLib geometry algorithms.
	@{
*/

/** @defgroup TopologicalOperators Topological operators
    @ingroup  GeometryAlgorithms
	Functions that test topologival relation between two objects.
\verbatim
	The topological operators are based on the intersection of interior, exterior and boundary of geometries:
  ------------------------------------------------------------------------------------------------------------------
 | TeGeometry   |  INTERIOR                        | BOUNDARY           | EXTERIOR                                |
  ------------------------------------------------------------------------------------------------------------------
 | TePoint      | The point itself                 | Empty              | Everything except interior              |
 | TeLine2D     | All points except the end points | The two end points | Everything except interior and boundary |
 | TeLinearRing | All points                       | Empty              | Everything except interior and boundary |
 | TePolygon    | Everything between the external  | All rings          | Everything except interior and boundary |
 |              | ring and the inner rings         |                    |                                         |
  ------------------------------------------------------------------------------------------------------------------
 
We use:
	- I: for Interior
	- E: for Exterior
	- B: for Boundary
	- inter: Intersects
	- ^: AND
	- v: OR
	- A = refer to two-dimensional geometries (TePolygon and TePolygonSet)
	- L = refer to one-dimensional geometries (TeLine2D and TeLineSet)
	- P = refer to 0-dimensional geometries   (TePoint and TePointSet)
 \endverbatim
@{
*/

/** @defgroup TeEquals Equals test
    @ingroup TopologicalOperators
    Check if one object is equal another object.
\verbatim
 Applies to: P/P, L/L and A/A.
 TeEquals(x, y) => (x inter y = x) ^ (y inter x = y)
                   (B(x) inter I(y) = false) ^ (B(x) inter E(y) = false) 
\endverbatim
@{
*/
//! If a specialized function is not used, returns false.
template<class T1, class T2>
inline bool TeEquals(const T1& /*o1*/, const T2& /*o2*/)
{
	return false;
}

//! Check if coordinate 1 and coordinate 2 are equal
template<>
TL_DLL bool TeEquals(const TeCoord2D& c1, const TeCoord2D& c2);

//!  Check if point 1 and point 2 are equal
template<>
TL_DLL bool TeEquals(const TePoint& p1, const TePoint& p2);

//! Check if lineRed and lineBlue are equal
template<>
TL_DLL bool TeEquals(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if polygon red and polygon blue are equal
template<>
TL_DLL bool TeEquals(const TePolygon& redPol, const TePolygon& bluePol);

//! Check if polygonset1 and polygonset1 are equal
template<>
TL_DLL bool TeEquals( const TePolygonSet& ps1, const TePolygonSet& ps2 );

//! Check if box 1 and box 2 are equal 
template<>
TL_DLL bool TeEquals(const TeBox& bx1, const TeBox& bx2);

//! Check if cell 1 and cell 2 are equal
template<>
TL_DLL bool TeEquals(const TeCell& cell1, const TeCell& cell2);
/** @} */ 

/** @defgroup TeDisjoint Disjoint test
    @ingroup TopologicalOperators
 Check if two objects are disjoint.
\verbatim
 Applies to all geometries.
 TeDisjoint(x, y) => (x inter y = false)
                     (I(x) inter I(y) = false) ^ (I(x) inter B(y) = false) ^ (B(x) inter I(y) = false) ^ (B(x) inter B(y) = false)
\endverbatim
@{
*/
//! Check if coordinate cl and coordinate c2 are disjoint
TL_DLL bool TeDisjoint(const TeCoord2D& c1, const TeCoord2D& c2);

//! Check if coordinate and box are disjoint
TL_DLL bool TeDisjoint(const TeCoord2D& c, const TeBox& b);

//! Check if box 1 and box 2 are disjoint
TL_DLL bool TeDisjoint(const TeBox& bx1, const TeBox& bx2);

//! Check if coordinate and line are disjoint
TL_DLL bool TeDisjoint(const TeCoord2D& c, const TeLine2D& l);

//! Check if coordinate and polygon are disjoint
TL_DLL bool TeDisjoint(const TeCoord2D& c, const TePolygon& pol);

//! Check if point l and point 2 are disjoint
TL_DLL bool TeDisjoint(const TePoint& p1, const TePoint& p2);

//! Check if point and object are disjoint
TL_DLL bool TeDisjoint(const TePoint& p, const TeLine2D& l);

//! Check if point and object are disjoint
TL_DLL bool TeDisjoint(const TePoint& p, const TePolygon& pol);

//! Check if lines are disjoint
TL_DLL bool TeDisjoint(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if line and polygon are disjoint
TL_DLL bool TeDisjoint(const TeLine2D& l, const TePolygon& pol);

//! Check if polygons are disjoint
TL_DLL bool TeDisjoint(const TePolygon& redPol, const TePolygon& bluePol);

//! Check if cell 1 and cell 2 are disjoint
TL_DLL bool TeDisjoint(const TeCell& cell1, const TeCell& cell2);

//! Check if cell and line are disjoint
TL_DLL bool TeDisjoint(const TeCell& cell, const TeLine2D& line);

//! Check if cell and polygon are disjoint
TL_DLL bool TeDisjoint(const TeCell& cell, const TePolygon& pol);

//! Check if cell and point are disjoint
TL_DLL bool TeDisjoint(const TeCell& cell, const TePoint& point);
/** @} */


/** @defgroup TeIntersects Intersects test
    @ingroup TopologicalOperators
	Check if one object intersects another object.
\verbatim
  Applies to all geometries.
  TeIntersects(x, y) => !TeDisjoint(x, y)
                     => (I(x) inter I(y) = true) v (I(x) inter B(y) = true) v (B(x) inter I(y) = true) v (B(x) inter B(y) = true)
\endverbatim
@{
*/
//! If a specialized function is not used, returns the disjoint negation

template<class T1, class T2>
bool TeIntersects(const T1& o1, const T2& o2)
{
	return !TeDisjoint(o1, o2);
}

//! Check if coordinate and box intersects
template<>
TL_DLL bool TeIntersects(const TeCoord2D& c, const TeBox& b);

//! Check if point and box intersects
template<>
TL_DLL bool TeIntersects(const TePoint& p, const TeBox& b);

//! Check if boxes intersects
template<>
TL_DLL bool TeIntersects(const TeBox& bx1, const TeBox& bx2);
/** @} */


/** @defgroup TeTouches Touch test
	@ingroup TopologicalOperators
	Check if two objects touches.
\verbatim
 Applies to A/A, L/L, L/A, P/A, P/L.
 TeTouches(x, y) => (I(x) inter I(y) = false) ^ (x inter y = true)
				    => (I(x) inter I(y) = false) ^ ((B(x) inter I(y) = true) v (I(x) inter B(y) = true) v (B(x) inter B(y) = true))
 \endverbatim
 @{
*/

//! Check if coordinate and line touches
TL_DLL bool TeTouches(const TeCoord2D& c, const TeLine2D& l);

//! Check if coordinate and polygon touches
TL_DLL bool TeTouches(const TeCoord2D& c, const TePolygon& pol);

//! Check if point and object touches
TL_DLL bool TeTouches(const TePoint& p, const TeLine2D& l);

//! Check if point and object touches
TL_DLL bool TeTouches(const TePoint& p, const TePolygon& pol);

//! Check if line and line touches
TL_DLL bool TeTouches(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if line and polygon
TL_DLL bool TeTouches(const TeLine2D& l, const TePolygon& pol);

//! Check if polygons touches
TL_DLL bool TeTouches(const TePolygon& redPol, const TePolygon& bluePol);

//! Check if boxes touches
TL_DLL bool TeTouches(const TeBox& bx1, const TeBox& bx2);

//! Check if cells touches
TL_DLL bool TeTouches(const TeCell& c1, const TeCell& c2);

//! Check if cell and line touches
TL_DLL bool TeTouches(const TeLine2D& line, const TeCell& cell);

//! Check if cell and polygon touches
TL_DLL bool TeTouches(const TeCell& c1, const TePolygon& poly);

//! Check if cell and point touches
TL_DLL bool TeTouches( const TePoint& point, const TeCell& c1);
/** @} */

/** @defgroup TeCrosses Crosses test
    @ingroup TopologicalOperators
	Check if one object crosses another object.
\verbatim
  TeCrosses(x, y) => dim(I(x) inter I(y)) = (max(dim(I(x)), dim(I(y))) - 1) ^ (x inter y != x) ^ (x inter y != y) 
  Case 1: x = TeLine2D and y = TePolygon
          => (I(x) inter I(y) = true) ^ (I(x) inter E(y) = true)
  Case 2: x = TeLine2D and y = TeLine2D
          => dim(I(x) inter I(y)) = 0
\endverbatim
@{
*/

//! Check if red line crosses blue line
TL_DLL bool TeCrosses(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if line crosses polygon
TL_DLL bool TeCrosses(const TeLine2D& l, const TePolygon& pol);

//! Check if line and cell crosses
TL_DLL bool TeCrosses(const TeLine2D& l, const TeCell& cell);
/** @} */


/** @defgroup TeWithin Within test
 *  @ingroup TopologicalOperators
 *  Check if one object is within another object.
 \verbatim
   TeWithin(x, y) => (x inter y = x) ^ (I(x) inter I(y) = true)
   Case 1: P/P, P/L and P/A
           => (I(x) inter I(y) = true)
   Case 2: L/L and A/A
           => (I(x) inter I(y) = true) ^ (I(x) inter E(y) = false) ^ (B(x) inter E(y) = false) ^ (B(x) inter B(y) = false)
   Case 3: L/A
           => (I(x) inter I(y) = true) ^ (I(x) inter E(y) = false) ^ (B(x) inter E(y) = false) ^ (B(x) inter B(y) = false) ^ (I(x) inter B(y) = false)
\endverbatim
 @{
*/

//! Check if coordinate 1 is within coordinate 2
TL_DLL bool TeWithin(const TeCoord2D& c1, const TeCoord2D& c2);

//! Check if coordinate is within a box
TL_DLL bool TeWithin(const TeCoord2D& c, const TeBox& b);

//! Check if a cordinate is within a line
bool TeWithin(const TeCoord2D& c, const TeLine2D& l);

//! Check if a cordinate is within a polygon
TL_DLL bool TeWithin(const TeCoord2D& c, const TePolygon& pol);

//! Check if point 1 is within point 2
TL_DLL bool TeWithin(const TePoint& p1, const TePoint& p2);

//! Check if point is within object
TL_DLL bool TeWithin(const TePoint& p, const TeLine2D& l);

//! Check if point is within object
TL_DLL bool TeWithin(const TePoint& p, const TePolygon& pol);

//! Check if red line is within blue line
TL_DLL bool TeWithin(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if line is within polygon
TL_DLL bool TeWithin(const TeLine2D& l, const TePolygon& pol);

//! Check if red polygon is within blue polygon
TL_DLL bool TeWithin(const TePolygon& redPol, const TePolygon& bluePol);

//! Check if box1 is within box2
TL_DLL bool TeWithin(const TeBox& bx1, const TeBox& bx2);

//! Check if cell1 is within cell2
TL_DLL bool TeWithin(const TeCell& cell1, const TeCell& cell2);

//! Check if line is within cell
TL_DLL bool TeWithin(const TeLine2D& line, const TeCell& cell);

//! Check if cell is within polygon
TL_DLL bool TeWithin(const TeCell& cell, const TePolygon& poly);

//! Check if point is within cell
TL_DLL bool TeWithin(const TePoint& point, const TeCell& cell);
/** @} */

/** @defgroup TeContains Contains test
    @ingroup TopologicalOperators
    Check if one object contains another object.
    TeContains(x, y) = TeWithin(y, x)
@{
*/

//! If a specialized function is not used, returns false
template<class T1, class T2>
inline bool TeContains(const T1& o1, const T2& o2)
{
	return TeWithin(o2, o1);
}
/** @} */

/** @defgroup TeOverlaps Overlaps test
	@ingroup TopologicalOperators
    Check if one object overlaps another object.
\verbatim
   TeOverlaps(x, y) => (dim(I(x)) = dim(I(y)) = dim(I(x) inter I(y))) ^ (x inter y != x) ^ (x inter y != y)
   Case 1: (x = TePolygon  and y = TePolygon)
           => (I(x) inter I(y) = true) ^ (I(x) inter E(y) = true) ^ (E(x) inter I(y) = true)
   Case 2: x = TeLine2D and y = TeLine2D
           => (dim(I(x) inter I(y)) = 1) ^ (I(x) inter E(y) = true) ^ (E(x) inter I(y) = true)
 \endverbatim
@{
*/

//! Check if red red overlaps blue line
TL_DLL bool TeOverlaps(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if red polygon overlaps blue polygon
TL_DLL bool TeOverlaps(const TePolygon& redPol, const TePolygon& bluePol);

//! Check if cell1 overlaps cell2
TL_DLL bool TeOverlaps(const TeCell& cell1, const TeCell& cell2);

//! Check if cell overlaps polygon
TL_DLL bool TeOverlaps(const TeCell& cell, const TePolygon& poly);
/** @} */


/** @defgroup TeCoveredBy Covered by test
  @ingroup TopologicalOperators
  Check if one object is covered by another object.
  \verbatim
 TeCoveredBy(x, y) => (x inter y = x) ^ (I(x) inter I(y) = true)
   - Case 1: (x = TePolygon and y = TePolygon) or (x = TeLine2D and y = TeLine2D)
             => (I(x) inter I(y) = true) ^ (I(x) inter E(y) = false) ^ (B(x) inter E(y) = false) ^ (B(x) inter B(y) = true)
   - Case 2: x = TeLine2D and y = TePolygon
             => (I(x) inter I(y) = true) ^ (I(x) inter E(y) = false) ^ (B(x) inter E(y) = false) ^ ((B(x) inter B(y) = true) v (I(x) inter B(y) = true))
 \endverbatim
 @{
 */

//! Check if red line is covered by blue line
TL_DLL bool TeCoveredBy(const TeLine2D& redLine, const TeLine2D& blueLine);

//! Check if line is covered by polygon
TL_DLL bool TeCoveredBy(const TeLine2D& l, const TePolygon& pol);

//! Check if red polygon is covered by blue polygon
TL_DLL bool TeCoveredBy(const TePolygon& redPol, const TePolygon& bluePol);

//! Check if cell1 is covered by cell2
TL_DLL bool TeCoveredBy(const TeCell& cell1, const TeCell& cell2);

//! Check if polygon is covered by cell
TL_DLL bool TeCoveredBy(const TePolygon& poly, const TeCell& cell);

//! Check if line is covered by cell
TL_DLL bool TeCoveredBy(const TeLine2D& line, const TeCell& cell);
/** @} */

/** @defgroup TeCovers Covers test
    @ingroup TopologicalOperators
    Check if one object covers another object.
    TeCovers(x, y) => TeCoveredBy(y, x)
@{
*/
//! Check if one object covers another object
template<typename T1, typename T2>
inline bool TeCovers(T1& o1, T2& o2)
{
	return TeCoveredBy(o2, o1);
}
/** @} */

/** @defgroup TeRelation Relation test
 *  @ingroup TopologicalOperators
 *  Return the relation between two objects.
 *  @{
 */
/** \brief Returns the relation between coordinate c and line l.
	\param c  The coordinate.
	\param l  The line.
	\return one of the relations: INSIDE, OUTSIDE or BOUNDARY.
	\note It doesn't do box elimination, just uses from TeIsOnLine(coordinate, line) elimination
 */
TL_DLL short TeRelation(const TeCoord2D& c, const TeLine2D& l);
 
 /** \brief Point in polygon inside/outside/boundary code.
	 \param c    The coordinate to test.
	 \param r    The simple polygon to test.
	 \return one of the relations: INSIDE, OUTSIDE or BOUNDARY.
	 \note	The ring is treated as a simple polygon (no holes). Does box elimination.
*/
TL_DLL short TeRelation(const TeCoord2D& c, const TeLinearRing& r);

/** \brief Coordinate in polygon inside/outside/boundary code.
	\param c    The coordinate to test.
	\param pol  The polygon to test.
	\return one of the relations: INSIDE, OUTSIDE or BOUNDARY.
	\note It doesn't do box elimination, just uses from TeRelation(coordinate, line) elimination
*/
TL_DLL short TeRelation(const TeCoord2D& c, const TePolygon& pol);

/** \brief Point in polygon inside/outside/boundary code.
	\param p    The coordinate to test.
	\param pol  The polygon to test.
	\return one of the relations: INSIDE, OUTSIDE or BOUNDARY.
	\note It doesn't do box elimination, just uses from TeRelation(coordinate, line) elimination
*/
TL_DLL short TeRelation(const TePoint& p, const TePolygon& pol); 

/** \brief Point in polygon set inside/outside/boundary code.
	\param c     The coordinate to test.
	\param pSet  The polygon set to test.
	\return one of the relations: INSIDE, OUTSIDE or BOUNDARY.
	\note Does box elimination.
*/
TL_DLL short TeRelation(const TeCoord2D& c, const TePolygonSet& pSet);

/** \brief This function returns the relation between two lines.
	\param lRed     The first line.
	\param lBlue    The second line.
	\param relation To return the relation that stop the search: TeDISJOINT, TeTOUCHES, TeWITHIN, TeCONTAINS, TeCROSSES, TeOVERLAPS, TeCOVEREDBY, TeCOVERS, TeEQUALS
	\note Doesn't do box elimination. You must implement the test in your own functions.
*/
TL_DLL short TeRelation(const TeLine2D& lRed, const TeLine2D& lBlue, const short& relation);

/** \brief This function returns the relation between a line and a polygon.
	\param line   The line.
	\param pol    The polygon.
	\return one of the relations:  TeDISJOINT, TeTOUCHES, TeWITHIN (THE LINE IS WITHIN), TeCROSSES, TeCOVEREDBY (THE LINE IS COVERED BY).
	\note Doesn't do box elimination. You must implement the test in your own functions.
*/
TL_DLL short TeRelation(const TeLine2D& line, const TePolygon& pol);

/** \brief This function returns the relation between two polygons.
	\param pRed   The first polygon.
	\param pBlue  The second polygon.
	\return one of the relations: TeEQUALS, TeDISJOINT, TeTOUCHES, TeWITHIN (pRed IS WITHIN pBlue), TeCONTANS (pBlue CONTAINS pRed), TeOVERLAPS, TeCOVEREDBY (pRed IS COVERED BY pBlue) or TeCOVERS (pRed COVERS pBlue).
	\note Doesn't do box elimination. You must implement the test in your own functions.
*/
TL_DLL short TeRelation(const TePolygon& pRed, const TePolygon& pBlue);
/** @}	*/
/** @}	*/

/** @defgroup BoxTests Special box tests
 *  @ingroup GeometryAlgorithms
 *  Box tests.
 *  @{
*/
//! Check if box1 is DISJOINT or TOUCHes box2
TL_DLL bool TeDisjointOrTouches(const TeBox& bx1, const TeBox& bx2);

//! Check if coordinate c is WITHIN or TOUCHes segments c1 and c2 
bool TeWithinOrTouches(const TeCoord2D& c, const TeCoord2D& c1, const TeCoord2D& c2);
/** @} */


/** @defgroup GeometryTests Special Geometry tests
 *  @ingroup GeometryAlgorithms
 *  Geometry tests.
 *  @{
*/
//! Check if geom1 is WITHIN, COVERED BY or EQUAL to geom2


template<class T1, class T2> 
bool TeWithinOrCoveredByOrEquals(const T1& geom1, const T2& geom2)
{
	short rel = TeRelation(geom1, geom2);

	if((rel&TeINSIDE) || (rel&TeBOUNDARY))
	   return true;

	return false;
}

//! Check if box1 is WITHIN, COVERED BY or EQUAL to box2
template<>
TL_DLL bool TeWithinOrCoveredByOrEquals(const TeBox& bx1, const TeBox& bx2);

//! Check if line1 is WITHIN, COVERED BY or EQUAL to line2
template<>
TL_DLL bool TeWithinOrCoveredByOrEquals(const TeLine2D& line1, const TeLine2D& line2);

//! Check if line1 is WITHIN or COVERED BY to pol
template<>
TL_DLL bool TeWithinOrCoveredByOrEquals(const TeLine2D& line1, const TePolygon& pol);

//! Check if pol1 is WITHIN, COVERED BY or EQUAL to pol2
template<>
TL_DLL bool TeWithinOrCoveredByOrEquals(const TePolygon& pol1, const TePolygon& pol2);
/** @} */


/** @defgroup IntersectionOperators Intersection Operators
 *  @ingroup  GeometryAlgorithms
 *  Functions that calculate the intersection among objects or do intersection test.
 *  @{
 */

/** \brief Performs the intersection between  box b1 and b2, returning the resulting box bout.
	\param bx1  The first box to do the intersection.
	\param bx2  The second box to do the intersection.
	\param bout The box formed by the intersection of bx1 and bx2.
*/
TL_DLL bool TeIntersection(const TeBox& bx1, const TeBox& bx2, TeBox& bout);

/** \brief Returns the segments that intersept the poly polygon in the line y.
	\param poly    A polygon.
	\param y    The ordinate that cuts the polygons edges.
*/
TL_DLL TeCoordPairVect	TeGetIntersections(const TePolygon &poly, const double& y);
/** @} */


/** @defgroup UnionOperators Union Operators
 *  @ingroup  GeometryAlgorithms
 *  Functions that compute the union of objects.
 *  @{
 */

/** \brief Combine two box, make one that includes both.
	\param bx1  The first box to do the union.
	\param bx2  The second box to do the union.
*/
TL_DLL TeBox TeUnion(const TeBox& bx1, const TeBox& bx2);

/** @} */

/** @defgroup TeLocationFunctions Functions that finds the localization of objects.
 *  @ingroup  GeometryAlgorithms
 *  Functions that finds the localization of objects.
 *  @{
 */
/** \brief Point in polygon inside/outside/boundary code.
	\param c    The coordinate to test.
	\param r The simple polygon to test.

	\note Check if point is INSIDE of a given ring.
	      The ring is treated as a simple polygon (no holes).
	      Adapted from:
		  Samosky, Joseph, "SectionView: A system for interactively specifying and
		  visualizing sections through three-dimensional medical image data,
		  M.S. Thesis, Department of Electrical Engineering and Computer Science,
		  Massachusetts Institute of Technology, 1993.
		 Obs: Doesn't do box elimination.
*/
TL_DLL bool TePointInPoly(const TeCoord2D& c, const TeLinearRing& r);

/** \brief Check if coordinate c is on segment (segment is closed).
	\param c   The coordinate to be tested.
	\param c1  The first segment's coordinate.
	\param c2  The second segment's coordinate.
*/
TL_DLL bool TeIsOnSegment(const TeCoord2D& c, const TeCoord2D& c1, const TeCoord2D& c2);

/** \brief Check if coordinate c is on line boundary or on line interior (see explanation above for definition of boundary and interior of a line).
	\param c   The coordinate to be tested.
	\param l   The line used in the test.

	Obs: Do box elimination.
*/
TL_DLL bool TeIsOnLine(const TeCoord2D& c, const TeLine2D& l);

/** \brief Locate the nearest line segment of a coordinate.
	\param pin		The coordinate.
	\param line		The line.
	\param segment  The position of the segment in the line  
	\param tol		Tolerance.
*/
TL_DLL bool TeLocateLineSegment(TeCoord2D& pin, TeLine2D& line, int& segment, double tol);
/** @} */

/** @defgroup TeConvexHull Functions to compute the Convex Hull
 *  @ingroup  GeometryAlgorithms
 *  Functions that returns the convex hull of a point list.
 *  @{
 */
 /** \brief Returns the convexhull of a given list of coords in counterclockwise.
	 \param coordSet A list with coordinates without duplicated coordinates.

	\note This algorithm is based on the book Computational Geometry
	      by M. de Berg, M. van Kreveld, M. Overmars and O. Schwarzkopf - Springer Verlag - pp. 6.
	      It is O(N log N).
*/
template<class T>
TePolygon TeConvexHull(const T& coordSet);

//! This is a explicit specialization that returns the convex hull of a TePolygon
template<>
TL_DLL TePolygon TeConvexHull(const TePolygon& p);

//! This is a explicit specialization that returns the convex hull of a TePolygonSet
template<>
TL_DLL TePolygon TeConvexHull(const TePolygonSet& ps);

//! This is a explicit specialization that returns the convex hull of a TePointSet. Must be defined!
template<>
TL_DLL TePolygon TeConvexHull(const TePointSet& ps);

/** @} */

/** @defgroup TeUtils Utilities functions.
 *  @ingroup  GeometryAlgorithms
 *  @{
 */
//! Given a projection "proj" returns a tolerance value in the same unit of projection to be used in geometric operations
TL_DLL double TeGetPrecision(TeProjection* proj);

//! This class implements the Epsilon-tweaking used in calculus.
class TL_DLL TeGeometryAlgorithmsPrecision
{
	protected:

		//! Constructor
		TeGeometryAlgorithmsPrecision();

	public:		

		//! Tells if d1 is greater than d2 according to tolerance factor.
		static bool IsGreater(const double& d1, const double& d2)
		{
			return ((d1 - d2) > TePrecision::instance().precision());
		}

		//! Tells if d1 is greater than or equal to d2 according to tolerance factor.
		static bool IsGreaterEqual(const double& d1, const double& d2)
		{
			return ((d1 - d2) >= (-TePrecision::instance().precision()));
		}

		//! Tells if d1 is smaller than d2 according to a tolerance factor.
		static bool IsSmaller(const double& d1, const double& d2)
		{
			return ((d1 - d2) < -(TePrecision::instance().precision()));
		}

		//! Tells if d1 is smaller than or equals to d2 according to a tolerance factor.
		static bool IsSmallerEqual(const double& d1, const double& d2)
		{
			return ((d1 - d2) <= TePrecision::instance().precision());
		}

		//! Tells if d1 is equals to d2 according to a tolerance factor.
		static bool IsEqual(const double& d1, const double& d2)
		{
			return (fabs(d1 - d2) <= TePrecision::instance().precision());
		}

		//! Tells if d1 is different from d2 according to a tolerance factor.
		static bool IsDifferent(const double& d1, const double& d2)
		{
			return (fabs(d1 - d2) > TePrecision::instance().precision());
		}
};

//! Removes the duplicate coordinates of a line
TL_DLL inline void TeRemoveDuplicatedCoordinates(TeLine2D& l)
{
	for(unsigned int i = 0; i < l.size() - 1; ++i)
		if(TeEquals(l[i], l[i + 1]))
		{
			l.erase(i);
			--i;
		}

	return;
}

//! Removes the duplicate coordinates of a polygon
TL_DLL inline void TeRemoveDuplicatedCoordinates(TePolygon& p)
{
	for(unsigned int i = 0; i < p.size(); ++i)
		TeRemoveDuplicatedCoordinates(p[i]);		

	return;
}

/** \brief Reverses the orientation of a line.
	\param lin  The line to be reversed.
*/
TL_DLL inline void TeReverseLine(TeLine2D& lin)
{
	for(unsigned int i=0,j=lin.size()-1 ; i<lin.size()/2 ; ++i,--j)
	{
		TeCoord2D p = lin[i];
		lin[i] = lin[j];
		lin[j] = p;
	}

	return;
}


/** \brief Verifies if a simple polygon defined as a linear ring is convex or not.
	\param ring  The polygon to test convexity.
*/
TL_DLL bool TeIsConvex(const TeLinearRing& ring);

/** \brief Returns the orientation of the ring (CLOCKWISE or COUNTERCLOCKWISE);
	\param r The ring to be checked.
*/
TL_DLL short TeOrientation(const TeLinearRing& r);

/** @} */

/** @defgroup MetricOperators Metric operators
 *  @ingroup  GeometryAlgorithms
 *  Functions that do some usefull metric operations.
 *  @{
 */

/** \brief Returns the middle point of a segment.
	\param first   The first segment's coordinate.
	\param last    The second segment's coordinate.
	\param middle  The middle point.
*/
TL_DLL void TeGetMiddlePoint(const TeCoord2D& first, const TeCoord2D& last, TeCoord2D& middle);

/** \brief Calculates the Euclidian distance between two points.
	\param c1 First coordinate;
	\param c2 Second coordinate;
*/
TL_DLL double TeDistance(const TeCoord2D& c1, const TeCoord2D& c2);

/** \brief Returns the length of a Line 2D.
	\param l  The line to calculate the length.	
*/
TL_DLL double TeLength(const TeLine2D& l);

/** \brief Perpendicular distance from point to segment.
	\param first  The first segment's coordinate.
	\param last   The second segment's coordinate.
	\param pin    The point to get the distance from the segment.
	\param pinter The point of intersection on the segment.
*/
TL_DLL double TePerpendicularDistance(const TeCoord2D& first, const TeCoord2D& last, const TeCoord2D& pin, TeCoord2D &pinter);

/** \brief Minimal distance from point to segment.
	\param first	The first segment's coordinate.
	\param last		The second segment's coordinate.
	\param pin		The point to get the minimal distance from the segment. This point is inside the segment  
	\param pout		The nearest segment point of the pin point.
	\param tol		Numerical tolerance
*/
TL_DLL double TeMinimumDistance (const TeCoord2D& first, const TeCoord2D& last, const TeCoord2D& pin, TeCoord2D& pout, double tol = 0.0);

/** \brief Template class to compute the area of a geometry
	\param geom The geometry whose area we want to known.
	\note This algorithm is based on the book Spatial Databases with Application to GIS
	      by Philippe Rigaux, Michel O. Scholl and Agnes Voisard.

*/

template<class T>
double TeGeometryArea(const T& /* geom */)
{
	return 0.0;
}

//! This is a explicit specialization that returns the area of a TePolygon
template<>
TL_DLL double TeGeometryArea(const TePolygon& p);

//! This is a explicit specialization that returns the area of a TePolygonSet
template<>
TL_DLL double TeGeometryArea(const TePolygonSet& ps);

//! This is a explicit specialization that returns the area of a Box
template<>
TL_DLL double TeGeometryArea(const TeBox& b);

//! This is a explicit specialization that returns the area of a Box
template<>
TL_DLL double TeGeometryArea(const TeMultiGeometry& mGeom);
/** @} */

/** @defgroup GeometryFunction Functions that return geometries.
 *  @ingroup  GeometryAlgorithms
 *  Functions that return geometries.
 *  @{
 */
/** \brief Given a box return its polygon representation.
	\param b  The box to create a polygon.
*/
TL_DLL TePolygon TeMakePolygon(const TeBox& b);

/** \brief Given N points, finds a path that doesn't self-intersects
	\note Given N points, finds a path that doesn't self-intersects, visiting all points and returning 
	      to the begginning one. It is based on the book Algorithms by Robert Sedgewick, Addisson-Wesley, 1988;
	\param pSet The point set to form a path.
*/
TL_DLL TeLinearRing TeSimpleClosedPath(const TePointSet& pSet);
/** @} */


/** @defgroup TeFindCentroid Functions to compute the centroid
 *  @ingroup  GeometryAlgorithms
 *  Functions that return the centroid.
 *  @{
 */

/** \brief Calculates the centroid of a multi geometry.
	\param p A multi geometry whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(TeMultiGeometry& mGeom);

/** \brief Calculates the centroid of a polygon.
	\param p A TePolygon whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TePolygon& p);

/** \brief Calculates a reference point.
	\param l A TeLine whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TeLine2D& l);  

/*! \fn TeCoord2D TeFindCentroid(const TeCell& c );
    \brief Calculates the centroid of a cell.
	\param c A TeCell whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TeCell& c);

/** \brief Calculates the centroid of a point.
	\param p A TePoint whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TePoint& p);

/** \brief Calculates the centroid of a text.
	\param t A TeText whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TeText& t);

/** \brief Calculates the centroid of a polygon set.
	\param s A TePolygon set whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TePolygonSet& s); 

/** Calculates the centroid of a line set.
	\param s A TeLine set whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TeLineSet& s); 

/** \brief Calculates the centroid of a cell set.
	\param s A TeCell set whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(const TeCellSet& s);

/** \brief Calculates the centroid of a point set.
	\param ps A TePointSet set whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(TePointSet& ps);

/** \brief Calculates the centroid of a text set.
	\param ts A TeTextSet set whose centroid we want to known.
*/
TL_DLL TeCoord2D TeFindCentroid(TeTextSet& ts);

/** \brief Calculates the center of a triangle.
	\param vert0 First triagle's vertex.
	\param vert1 Second triagle's vertex.
	\param vert2 Third triagle's vertex.
	\param pc    The triangle center.
*/
TL_DLL bool TeFindTriangleCenter(const TeCoord2D& vert0, const TeCoord2D& vert1, const TeCoord2D& vert2, TeCoord2D& pc); 
/** \brief Calculates intersection between two segments.
	\param fr0 First point of first segment.
	\param to0 Second point of first segment.
	\param fr1 First point of second segment.
	\param to1 Second point of second segment.
	\param pi    The intersection point coordinates.
	\return true is intersects, false otherwise.
*/

TL_DLL bool TeSegmentsIntersectPoint(const TeCoord2D& fr0, const TeCoord2D& to0, const TeCoord2D& fr1, const TeCoord2D& t1, TeCoord2D& pi); 

/** @} */


/** @defgroup TeNearest Functions to compute the nearest part of an object to a point.
  *  @ingroup  GeometryAlgorithms
  *  Auxiliary functions.
  *  @{
  */
//! Nearest node in set from location pt (i = the node index in the nodeset)
TL_DLL bool TeNearest(TeCoord2D& pt, TeNodeSet& ns , int& i, const double& tol = 0.0);

//! Nearest point in set from location pt (i = the point index in the pointset)
TL_DLL bool TeNearest(TeCoord2D& pt, TePointSet& ps , int& i, const double& tol = 0.0);

//! Nearest text in set from location pt (i = the text index in the textset)
TL_DLL bool TeNearest(TeCoord2D& pt, TeTextSet& ts , int& i, const double& tol = 0.0);

//! Nearest line in set from location pt (i = the line index in the lineset and pi = the closest point)
//! The pi point can be outside a line of the line set
TL_DLL bool TeNearest(TeCoord2D& pt, TeLineSet& ls , int& i, TeCoord2D& pi, const double& tol);

//! Nearest line in set from location pt (i = the line index in the lineset and pout = the closest point)
//! The pout point must be inside a line of the line set
TL_DLL bool TeNearest (TeCoord2D& pt,TeLineSet& ls, int& lindex, TeCoord2D& pout, double& dmin, const double& tol = 0.0); 

//! Nearest polygon in set from location pt (i = the polygon index in the polygonset).
TL_DLL bool TeNearest(TeCoord2D& pt, TePolygonSet& ps , int& i, const double& tol = 0.0);

//! Nearest line in set from location pt (i = the line index in lineset) calculated by the vertex of the lines
TL_DLL bool TeNearestByPoints(TeCoord2D& pt, TeLineSet& ls , int& i, double& dist, const double& tol = 0.0);
/** @} */

/** @defgroup CurveFunctions Functions used to deal with smooth curves
  *  @ingroup  GeometryAlgorithms
  *  @{
  */
/** \brief Given three points of a circunference, returns its center point.
	\param p1      First point.
	\param p2      Second point.
	\param p3      Third point.
	\param center  Circunference center.
	This algorithm is adapted from http://www.delphiforfun.org/Programs/Math_Topics/circle_from_3_points.htmbook.
*/
TL_DLL bool TeGetCenter(TePoint p1, TePoint p2, TePoint p3, TePoint &center);

/** \brief Given three points of a circunference, returns the radius.
	\param p1      First point.
	\param p2      Second point.
	\param p3      Third point.
	This algorithm is adapted from http://www.delphiforfun.org/Programs/Math_Topics/circle_from_3_points.htmbook.
*/
TL_DLL double TeGetRadius(TePoint &p1, TePoint &p2, TePoint &p3);

/** \brief Given three points returns a smooth arc as a TeLine2D that contains a given total number of points.

	\param p1      First point.
	\param p2      Second point.
	\param p3      Third point.
	\param arcOut  The return arc.
	\param NPoints Number of arc points.	
	
	This algorithm is adapted from http://mathforum.org/dr.math/
  
*/
TL_DLL bool TeGenerateArc(TePoint& p1, TePoint& p2, TePoint& p3, TeLine2D& arcOut, const short& NPoints);

/** \brief Given a center point and a radius, returns the circle  as a TeLine2D interpolated by a given number of points
	\param center  Center point of the circle.
	\param radius  radius of the circle.
	\param circle  The return circle
	\param NPoints Number of circle points.	
*/
TL_DLL bool TeGenerateCircle(TePoint& center, const double& radius, TeLine2D& circle, const short& NPoints);

/** \brief Performs a line simplication
	\param line		The line to be simplified
	\param snap		Simplification threshold
	\param maxdist	The maximum distance between intermediary segments
*/
TL_DLL bool TeLineSimplify(TeLine2D& line, double snap, double maxdist);


/** @} */

/** @} */ // end of group  GeometryAlgorithms


#endif	// __TERRALIB_INTERNAL_GEOMETRYALGORITHMS_H
