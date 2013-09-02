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
/*! \file   TeBufferRegion.h
	\brief  This file contains structures and definitions for buffer region (map distance) operation on objects.
	\author Gilberto Ribeiro de Queiroz <gribeiro@dpi.inpe.br>
 */

#ifndef  __TERRALIB_INTERNAL_BUFFER_H
#define  __TERRALIB_INTERNAL_BUFFER_H

#include "TeCoord2D.h"

class TeLine2D;
class TePolygon;
class TePolygonSet;
class TeLineSet;
class TePointSet;
class TeCellSet;

/*! \brief Contains structures and definitions needed to execute Buffer Region operation
*/
namespace TeBUFFERREGION
{	
	//!	Buffer type.
	/*!
		- TeINSIDEBUFFER			used to make only inside buffer lines
		- TeOUTSIDEBUFFER			used to make only outside buffer lines
		- TeINSIDEOUTSIDEBUFFER		used to make inside and outside buffer lines
	*/ 
	enum TeBufferType { TeINSIDEBUFFER, TeOUTSIDEBUFFER, TeINSIDEOUTSIDEBUFFER };

	/** \brief Calculates a buffer region around a point (DEPRECATED: use TeBufferRegion with buffer levels instead).
		\param coord			the location of the point
		\param bufferDistance	the desired distance in the same unit as coord
		\param numPoints		the number of points that should be used to describe the buffer zone (a circle in this case)
		\param pol				to return the buffer zone: a circle polygon in clockwise order
		\return Returns true if operation succeed otherwise returns false.
	*/
	TL_DLL bool TeBufferRegion(const TeCoord2D& coord, const double& bufferDistance, const unsigned int& numPoints, TePolygon& pol);

	/** \brief Calculates a buffer region around a point.
		\param coord			the location of the point
		\param bufferDistance	the desired distance in the same unit as coord
		\param numPoints		the number of points that should be used to describe the buffer zone (a circle in this case)
		\param bufferLevels		the number of buffer intervals
		\param bufferPols		to return the buffer intervals: a circle polygon in clockwise order with holes in counterclockwise order
		\return Returns true if operation succeed otherwise returns false.
	*/
	TL_DLL bool TeBufferRegion(const TeCoord2D& coord, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygon>& bufferPols);

	/** \brief Calculates a buffer region around a line (DEPRECATED: use TeBufferRegion with buffer levels instead).
		\param line				the line 
		\param bufferDistance	the desired distance in the same unit as line coordinates
		\param numPoints		the number of points that should be used to describe the curved corners of the buffer zone
		\param ps				to return the buffer zone: a set of polygons
		\return Returns true if operation succeed otherwise returns false.
	*/	
	TL_DLL bool TeBufferRegion(const TeLine2D& line, const double& bufferDistance, const unsigned int& numPoints, TePolygonSet& ps);
	
	/** \brief Calculates a buffer region around a line.
		\param line				the line 
		\param bufferDistance	the desired distance in the same unit as line coordinates
		\param numPoints		the number of points that should be used to describe the curved corners of the buffer zone
		\param bufferLevels		the number of buffer intervals
		\param polVec			to return the buffer zone: a vector of polygons
		\return Returns true if operation succeed otherwise returns false.
	*/	
	TL_DLL bool TeBufferRegion(const TeLine2D& line, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polVec);

	
	/** \brief Calculates a buffer region around a polygon (DEPRECATED: use TeBufferRegion with buffer levels instead).
		\param pol				the polygon 
		\param bufferDistance	the desired distance in the same unit as polygon rings coordinates
		\param numPoints		the number of points that should be used to describe the curved corners of the buffer zone
		\param ps				to return the buffer zone: a set of polygons
		\return Returns true if operation succeed otherwise returns false.
	*/	
	TL_DLL bool TeBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, TePolygonSet& ps);

	/** \brief Calculates a buffer region around a polygon (DEPRECATED: use TeBufferRegion with buffer levels instead).
		\param pol				the polygon 
		\param bufferDistance	the desired distance in the same unit as polygon rings coordinates
		\param numPoints		the number of points that should be used to describe the curved corners of the buffer zone
		\param bufferLevels		the number of buffer intervals
		\param buffType		    the buffer type: inside/outside/inside+outside
		\param polsVec			to return the buffer zone: a vector of polygon set
		\return Returns true if operation succeed otherwise returns false.
	*/	
	TL_DLL bool TeBufferRegion(const TePolygon& pol, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, const TeBufferType& buffType, vector<TePolygonSet>& polsVec);

	/** \brief Calculates a buffer region around an object polygon set.
		\param polSetIn				the object polygon set
		\param bufferDistance		the desired distance in the same unit as polygon rings coordinates
		\param numPoints			the number of points that should be used to describe the curved corners of the buffer zone
		\param bufferLevels			the number of buffer intervals
		\param bufferType			the buffer type: inside/outside/inside+outside
		\param polyVecOut			to return the buffers of the object, each level in a position of the vector
		\return Returns true if operation succeed otherwise returns false.
	*/	
	TL_DLL bool TeBufferRegion(const TePolygonSet& polSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, const TeBufferType& bufferType, vector<TePolygonSet>& polyVecOut);

	/** \brief Calculates a buffer region around an object line set.
		\param lineSetIn			the object line set
		\param bufferDistance		the desired distance in the same unit as lines coordinates
		\param numPoints			the number of points that should be used to describe the curved corners of the buffer zone
		\param bufferLevels			the number of buffer intervals
		\param polyVecOut			to return the buffers of the object, each level in a position of the vector
		\return Returns true if operation succeed otherwise returns false.
	*/	
	TL_DLL bool TeBufferRegion(const TeLineSet& lineSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polyVecOut);

	/** \brief Calculates a buffer region around an object point set.
		\param pointSetIn			the object point set
		\param bufferDistance		the desired distance in the same unit as points coordinates
		\param numPoints			the number of points that should be used to describe the curved corners of the buffer zone
		\param bufferLevels			the number of buffer intervals
		\param polyVecOut			to return the buffers of the object, each level in a position of the vector
		\return Returns true if operation succeed otherwise returns false.
	*/			
	TL_DLL bool TeBufferRegion(const TePointSet& pointSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, vector<TePolygonSet>& polyVecOut);
	
	/** \brief Calculates a buffer region around an object cell set.
		\param cellSetIn			the object cell set
		\param bufferDistance		the desired distance in the same unit as cells coordinates
		\param numPoints			the number of points that should be used to describe the curved corners of the buffer zone
		\param bufferLevels			the number of buffer intervals
		\param bufferType			the buffer type: inside/outside/inside+outside
		\param polyVecOut			to return the buffers of the object, each level in a position of the vector
		\return Returns true if operation succeed otherwise returns false.
	*/			
	TL_DLL bool TeBufferRegion(const TeCellSet& cellSetIn, const double& bufferDistance, const unsigned int& numPoints, const unsigned int& bufferLevels, const TeBufferType& bufferType, vector<TePolygonSet>& polyVecOut);

}	// end namespace TeBUFFER

#endif //__TERRALIB_INTERNAL_BUFFER_H

