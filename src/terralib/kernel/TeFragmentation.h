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
/*! \file TeFragmentation.h
    \brief This file contains Algorithms for Topological Operations.
	       These data structures and algorithms MUST BE USED ONLY BY TerraLib kernel and should NOT be used by anyone because
           THIS IS FOR INTERNAL USE ONLY.
*/

#ifndef  __TERRALIB_INTERNAL_FRAGMENTATION_H
#define  __TERRALIB_INTERNAL_FRAGMENTATION_H

#include "TeGeometry.h"
#include "TeIntersector.h"

/** @defgroup FragmentAlgorithms Algorithms that performs fragmentation of lines
	@ingroup GeometryAlgorithms
    THIS IS FOR INTERNAL USE ONLY: TerraLib fragmentation algorithms. 
 *  @{
 */
/** \brief Fragments the lines according intersection points (May rise an exception: you must use try and catch).
	\param lines			  The lines to fragment.
	\param ips			      The intersection points where the boundary will be fragmented.
	\param boundaryFragments  Output list of segments in the boundary of another line.
	\param fragments		  Output list of line fragmemts.
*/
TL_DLL void TeFragmentBoundary(const TeLineSet& lines, TeINTERSECTOR2::TeVectorBoundaryIP& ips, TeLineSet& boundaryFragments, TeLineSet& fragments);

/**  Fragments the lines according intersection points (May rise an exception: you must use try and catch).	
	\param polygons              List of polygons (lines to be fragmented).
	\param ips				     Intersection points where the boundary will be fragmented.
	\param boundaryFragments     Output list of segments in the boundary of another line.
	\param boundaryFragmentsIds  Output list with the objects identifiers for each fragment (to be used by location, to decide in/out fragments).
	\param fragments		     Output list of line fragmemts.
	\param fragmentsIds		     Output list with the objects identifiers for each fragment (to be used by location, to decide in/out fragments).
*/
TL_DLL void TeFragmentBoundary(const TePolygonSet& polygons, TeINTERSECTOR2::TeVectorBoundaryIP& ips, 
						TeLineSet& boundaryFragments, vector<pair<unsigned int, unsigned int> >& boundaryFragmentsIds, TeLineSet& fragments, vector<pair<unsigned int, unsigned int> >& fragmentsIds);

/** @} */ // end of group  FragmentAlgorithms

#endif	// __TERRALIB_INTERNAL_FRAGMENTATION_H
