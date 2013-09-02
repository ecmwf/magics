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
/*! \file TeOverlay.h
    \brief This file contains structures and definitions for set operation on polygons.
	\author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
 */

#ifndef  __TERRALIB_INTERNAL_OVERLAY_H
#define  __TERRALIB_INTERNAL_OVERLAY_H

#include "TeMultiGeometry.h"

/**
 * @brief Contains structures and definitions needed to execute Set Operations for polygons and lines (union, intersection e difference).
 */

namespace TeOVERLAY
{	
/** @defgroup SetOperations Set Operations
 *  Set operations for polygons and lines: intersection, union or difference.
 *  @{
 */

/** \brief Executes one of the possible Set Operations on polygons. 
    \param redPols		The first list of disjoint polygons (and without self-intersections).
	\param bluePols		The second list of disjoint polygons (and without self-intersections).
	\param polsOut		The polygon list from the operation result.
	\param operation	Type of operation: TeUNION, TeINTERSECTION or TeDIFFERENCE.
	\return				True if the operation success and false if an error has ocurried.
	
	\note Each polygon list must have disjoint polygons, otherwise, the result is undefined. This function must not be called directly,
	      instead of call it, choose a specific function below, because, this signature is deprecated and will change in near future.
*/
TL_DLL bool TeOverlay(const TePolygonSet& redPols, const TePolygonSet& bluePols, TePolygonSet& polsOut, const short& operation);

/** \brief Calculates the union Set Operation for polygons.
    \param redPols		The first list of disjoint polygons (and without self-intersections).
	\param bluePols		The second list of disjoint polygons (and without self-intersections).
	\param polsOut		The polygon list from the operation result.
	\return				True if the operation success and false if an error has ocurried.

*/
TL_DLL bool TeUnion(TePolygonSet& redPols, TePolygonSet& bluePols, TePolygonSet& polsOut);


/** \brief Calculates the  intersection Set Operation for polygons.
    \param redPols      The first list of disjoint polygons (and without self-intersections).
	\param bluePols		The second list of disjoint polygons (and without self-intersections).
	\param polsOut      The polygon list from the operation result.
	\return				True if the operation success and false if an error has ocurried.

*/
TL_DLL bool TeIntersection(TePolygonSet& redPols, TePolygonSet& bluePols, TePolygonSet& polsOut);


/** \brief Calculates the difference set operation for polygons (redPols minus bluePols).
    \param redPols      The first list of disjoint polygons (and without self-intersections).
	\param bluePols		The second list of disjoint polygons (and without self-intersections).
	\param polsOut      The polygon list from the operation result.
	\return				True if the operation success and false if an error has ocurried.

*/
TL_DLL bool TeDifference(TePolygonSet& redPols, TePolygonSet& bluePols, TePolygonSet& polsOut);


/** \brief Executes one of the possible Set Operations for lines and polygons. 
    \param redLines		A disjoint line list (and without self-intersections).
	\param bluePols		A list of disjoint polygons (and without self-intersections).
	\param operation    Type of operation: TeUNION, TeINTERSECTION or TeDIFFERENCE.
	\return				A geometry that can contains only lines on intersection and difference cases, and lines + polygons on union case.

	\note This function must not be called directly, instead of call it, choose a specific function below,
	      because, this signature is deprecated and will change in near future.

*/
TL_DLL TeMultiGeometry TeOverlay(const TeLineSet& redLines, const TePolygonSet& bluePols, const short& operation);


/** \brief Calculates the union set operation for lines and polygons.
    \param redLines     A disjoint line list (and without self-intersections).
	\param bluePols		A list of disjoint polygons (and without self-intersections).
	\return				A geometry formed by lines and polygons or only by polygons.

*/
TL_DLL TeMultiGeometry TeUnion(TeLineSet& redLines, TePolygonSet& bluePols);


/** \brief Calculates the intersection set operation for lines and polygons.
    \param redLines     A disjoint line list (and without self-intersections).
	\param bluePols		A list of disjoint polygons (and without self-intersections).
	\return				A geometry formed only by lines or empty.

*/
TL_DLL TeMultiGeometry TeIntersection(TeLineSet& redLines, TePolygonSet& bluePols);


/** \brief Calculates the defference set operation for lines and polygons.
    \param redLines     A disjoint line list (and without self-intersections).
	\param bluePols		A list of disjoint polygons (and without self-intersections).
	\return				A geometry formed only by lines or empty.

*/
TL_DLL TeMultiGeometry TeDifference(TeLineSet& redLines, TePolygonSet& bluePols);


/** \brief Calculates the union of all polygons passed in polsIn parameter, and returns a polygon set (polsOut).
    \param polsIn            A list of polygons, may have overlap (disjoint or not).
	\param polsOut           Where the union will be stored.
	\param makeCopy          If true, before to start the union process a copy will be done for all coordinates, otherwise, the rings and coordinates of the input polygon will be used and so, the input can be invalidate (because of handle/body share of implementaton) bu this will save some memory.
	\param fixOrientation    If true, the ring orientation is checked, otherwise, it is assumed that they are in correct order of processing (you may set to false only if you know implementation details otherwise an error may occur).
*/
TL_DLL bool TeUnion(TePolygonSet& polsIn, TePolygonSet& polsOut, const bool& makeCopy = true, const bool fixOrientation = true);


/** \brief Calculates the intersection set operation for polygons. This version of intersection will reuse some pre-processing of redPols, and will do intersection between redPols and each polygonset in bluePols vector. The result of each individual operation (redPols x a polygonset from the blue vector) will be store in the output vector in the same order of polygonsets in bluePols. If an intersection is empty, an empty polygonset will be stored to indicate this.
    \param redPols			A list of disjoint polygons (and without self-intersections) used as a mask to cut each polygonset in bluePols vector.
	\param bluePols			A vector that contains polygonsets that will be individually cutted by redPols, and each blue polygonset must be disjoint (and without self-intersections).
	\param vecPolsOut       A vector with the result of intersection between redPols and each polygonset in the blue polygonset vector.
	\param resultVec        A vector of bool that tells if individual intersection succeed or not.
	\param makeCopy         If true, before to start the union process a copy will be done for all coordinates, otherwise, the rings and coordinates of the input polygon will be used and so, the input can be invalidate (because of handle/body share of implementaton) bu this will save some memory.
	\param fixOrientation   If true, the ring orientation is checked, otherwise, it is assumed that they are in correct order of processing (you may set to false only if you know implementation details otherwise an error may occur).
	\return				True if the operation success and false if an error has ocurried.

	\note The output vector may contains polygonsets thar are empty, only to indicate that intersection was empty, so be carefull.

*/
TL_DLL bool TeIntersection(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const bool& makeCopy = true, const bool fixOrientation = true);


/** \brief Calculates the union set operation for polygons.
           This version of union will reuse some pre-processing of redPols,
		   and will do union between redPols and each polygonset in bluePols.
		   The result of each individual operation (redPols x a polygonset from the blue vector)
		   will be store in the output vector in the same order of polygonsets in bluePols.
    \param redPols			A list of disjoint polygons (and without self-intersections) used as a mask to cut each polygonset in bluePols vector.
	\param bluePols			A vector that contains polygonsets that will be individually cutted by redPols, and each blue polygonset must be disjoint (and without self-intersections).
	\param vecPolsOut       A vector with the result of union between redPols and each polygonset in the blue polygonset vector.
	\param resultVec        A vector of bool that tells if individual intersection succeed or not.
	\param makeCopy         If true, before to start the union process a copy will be done for all coordinates, otherwise, the rings and coordinates of the input polygon will be used and so, the input can be invalidate (because of handle/body share of implementaton) bu this will save some memory.
	\param fixOrientation   If true, the ring orientation is checked, otherwise, it is assumed that they are in correct order of processing (you may set to false only if you know implementation details otherwise an error may occur).
	\return				True if the operation success and false if an error has ocurried.
*/
TL_DLL bool TeUnion(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const bool& makeCopy = true, const bool fixOrientation = true);


/** \brief Calculates the difference set operation for polygons.
           This version of difference will reuse some pre-processing of redPols,
		   and will do difference between redPols and each polygonset in bluePols vector.
		   The result of each individual operation (redPols x a polygonset from the blue vector)
		   will be store in the output vector in the same order of polygonsets in bluePols.
		   If an intersection is empty, an empty polygonset will be stored to indicate this.
    \param redPols			A list of disjoint polygons (and without self-intersections) used as a mask to cut each polygonset in bluePols vector.
	\param bluePols			A vector that contains polygonsets that will be individually cutted by redPols, and each blue polygonset must be disjoint (and without self-intersections).
	\param vecPolsOut       A vector with the result of differnce between redPols and each polygonset in the blue polygonset vector.
	\param resultVec        A vector of bool that tells if individual intersection succeed or not.
	\param makeCopy         If true, before to start the union process a copy will be done for all coordinates, otherwise, the rings and coordinates of the input polygon will be used and so, the input can be invalidate (because of handle/body share of implementaton) bu this will save some memory.
	\param fixOrientation   If true, the ring orientation is checked, otherwise, it is assumed that they are in correct order of processing (you may set to false only if you know implementation details otherwise an error may occur).
	\return				True if the operation success and false if an error has ocurried.

	\note The output vector may contains polygonsets thar are empty, only to indicate that intersection was empty, so be carefull.
*/
TL_DLL bool TeDifference(TePolygonSet& redPols, vector<TePolygonSet>& bluePols, vector<TePolygonSet>& vecPolsOut, vector<bool>& resultVec, const bool& makeCopy = true, const bool fixOrientation = true);

/** \brief Calculates the union of all polygons passed in polsIn parameter,
           and returns a polygon set (polsOut).
	       This type of union use a different strategy from the
		   previous one: do union for each pair and
		   then repeat until we have only two pairs.
    \param psetIn            A list of polygons, may have overlap (disjoint or not).
	\param psetOut           Where the union will be stored.
 */
TL_DLL bool TePairUnion(TePolygonSet& psetIn, TePolygonSet& psetOut);

/** \brief Valid polygon holes: if they have a commom edge,
           they will be joined (THIS IS FOR INTERNAL USE ONLY).
	\param  polygon		The polygon to check holes.
	\param  psValid		The result polygon.
	\return				Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeValidPolygonHoles(TePolygon& polygon, TePolygonSet& psValid);

/** \brief Valid polygon holes: if they have a commom edge, they will be joined (THIS IS FOR INTERNAL USE ONLY).
	\param  polygons	The polygons to check holes.
	\param  psValid		The result polygon.
	\return				Returns true if the operation successed otherwise returns false.
 */
TL_DLL bool TeValidPolygonHoles(TePolygonSet& polygons, TePolygonSet& psValid);

/** @} */ 

}	// end namespace TeOVERLAY

#endif //__TERRALIB_INTERNAL_OVERLAY_H

