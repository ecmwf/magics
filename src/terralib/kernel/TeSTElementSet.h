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
/*! \file TeSTElementSet.h
	\brief This file contains structures to deal with a set of spatio-temporal 
	instances. These instances can belong to a specific layer or theme.
*/

#ifndef  __TERRALIB_INTERNAL_STELEMENTSET_H
#define  __TERRALIB_INTERNAL_STELEMENTSET_H

#include "TeBaseSTInstanceSet.h"
#include "TeSTInstance.h"
#include "TeTimeInterval.h"
#include "TeMultiGeometry.h"

class TeQuerier;
class TeTheme;
class TeLayer;


/*! \class TeSTElementSet
	\brief A class that represents a set of spatial temporal instances.

	This class specializes the abstract class TeBaseSTInstanceSet representing
	each spatial temporal instance of the set as a TeSTInstance type. That is, the 
	geometries of each instance in this set is represented as multi geometries 
	(TeMultiGeometry class)and its valid time as a time interval (TeTimeInterval class). 

	\sa TeBaseSTInstanceSet TeSTInstance TeMultiGeometry TeTimeInterval TeTheme TeLayer
*/
class TL_DLL TeSTElementSet  : public TeBaseSTInstanceSet<TeMultiGeometry, TeTimeInterval, TeSTInstance>
{
protected:

	//! Builds the set using a given querier and a specific slice
	bool buildImpl(TeQuerier* querier, const int& slide = -1);

public:
	//! Constructor
	TeSTElementSet() : TeBaseSTInstanceSet<TeMultiGeometry, TeTimeInterval, TeSTInstance>()
	{ }

	//! Constructor 
	TeSTElementSet(TeTheme* theme, TeAttributeList attList = TeAttributeList()) : 
		TeBaseSTInstanceSet<TeMultiGeometry, TeTimeInterval, TeSTInstance>(theme, attList)
	{ }
	
	//! Constructor 
	TeSTElementSet(TeLayer* layer, TeAttributeList attList = TeAttributeList()) : 
		TeBaseSTInstanceSet<TeMultiGeometry, TeTimeInterval, TeSTInstance>(layer, attList)
	{ }


	//! Constructor 
	TeSTElementSet(const TeBox& box, const TeAttributeList& attrList) :
		TeBaseSTInstanceSet<TeMultiGeometry, TeTimeInterval, TeSTInstance>(box, attrList)
	{ }


	//! Returns the valid time interval for all ST instances
	TeTimeInterval totalTime(); 

	//! Sets the valid time interval for all ST instances
	void totalTime(TeTimeInterval t); 

	//! Deprecated: Verifies if there is a specific element or object in the set 
	bool hasElement(const string& objId);

	//! Sets a polygon set and its valid time interval to a specific object or element
	bool setGeometry(const string& object_id, const TePolygonSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Sets a line set and its valid time interval to a specific object or element
	bool setGeometry(const string& object_id, const TeLineSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Sets a point set and its valid time interval to a specific object or element
	bool setGeometry(const string& object_id, const TePointSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Sets a cell set and its valid time interval to a specific object or element
	bool setGeometry(const string& object_id, const TeCellSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Sets a text set and its valid time interval to a specific object or element
	bool setGeometry(const string& object_id, const TeTextSet& geomSet, TeTimeInterval time = TeTimeInterval()); 

	//! Gets a polygon set of an element or object and its valid time interval
	bool getGeometry(const string& object_id, TePolygonSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Gets a line set of an element or object and its valid time interval
	bool getGeometry(const string& object_id, TeLineSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Gets a point set of an element or object and its valid time interval
	bool getGeometry(const string& object_id, TePointSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Gets a cell set of an element or object and its valid time interval
	bool getGeometry(const string& object_id, TeCellSet& geomSet, TeTimeInterval time = TeTimeInterval());
	//! Gets a text set of an element or object and its valid time interval
	bool getGeometry(const string& object_id, TeTextSet& geomSet, TeTimeInterval time = TeTimeInterval());
};


/*! \example createSTElementSetFromLayer.cpp
	Shows how to create a spatio-temporal element set from a layer.
 */

/*! \example createSTElementSetFromShapeFile.cpp
	Shows how to create aspatio-temporal element set from a shapefile.
 */

/*! \example createSTElementSetFromTheme.cpp
	Shows how to create aspatio-temporal element set from a theme.
 */
#endif

