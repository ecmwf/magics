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
/*! \file TeSTInstance.h
	\brief This file contains a class called TeSTInstance that represents 
	an instance in time of a geographical object or element.  
*/

#ifndef  __TERRALIB_INTERNAL_STINSTANCE_H
#define  __TERRALIB_INTERNAL_STINSTANCE_H

#include "TeTimeInterval.h"
#include "TeMultiGeometry.h"
#include "TeBaseSTInstance.h"

#include <string>
#include <map> 
#include <vector>
using namespace std;

/*! \class TeSTInstance
	\brief A class that represents an instance in a time of a spatial object.

	A spatio-temporal instance (STInstance) is composite of an attribute set and geometries 
	of a spatial element or object that are valid in a specific time. This class 
	specializes the base class TeBaseSTInstance representing the geometries 
	as multigeometries (TeMultiGeometry) and the valid time as a time interval (TeTimeInterval).

	\sa TeBaseSTInstance TeMultiGeometry TeTimeInterval
	
*/
class TL_DLL TeSTInstance : public TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>
{
public:		

	//! Constructor
	TeSTInstance() : 
		TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>() 
	{ }

	//! Deprecated: Constructor
	TeSTInstance (const string& object_id, TeProperty& prop); 

	//! Constructor 
	TeSTInstance (const string& object_id, vector<string>& prop, TeAttributeList* attList = 0, const int& s = -1) : 
		TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>(object_id, prop, attList, s)
	{ }

	//! Constructor
	TeSTInstance (const string& object_id, const TeMultiGeometry& geometries, 
		const TeTimeInterval& time, const int& s = -1) : 
		TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>(object_id, geometries, time, s)
	{ }

	//! Constructor 
	TeSTInstance (const string& object_id, vector<string>& prop, TeAttributeList* attList, 
		TeMultiGeometry& geometries, int& slice, TeTimeInterval& time ) :
		TeBaseSTInstance<TeMultiGeometry, TeTimeInterval>(object_id, prop, attList, geometries, slice, time)
	{ }

	//! Clear 
	virtual void clear(); 
	
	//! Verifies if the its time is valid
	virtual bool isTimeValid();
	
	//! Deprecated: Verifies if the instance has polygons
	bool hasPolygons();	
	//! Deprecated: Verifies if the instance has lines
	bool hasLines();		
	//! Deprecated: Verifies if the instance has points
	bool hasPoints();		
	//! Deprecated: Verifies if the instance has cells
	bool hasCells();		
	//! Deprecated: Return true if this has texts
	bool hasTexts();		
	
	//! Deprecated: Gets a copy of the ST instance polygon set
	bool getGeometry(TePolygonSet& result); 
	//! Deprecated: Gets a copy of the ST instance line set
	bool getGeometry(TeLineSet& result);
	//! Deprecated: Gets a copy of the ST instance point set
	bool getGeometry(TePointSet& result);
	//! Deprecated: Gets a copy of the ST instance cell set
	bool getGeometry(TeCellSet& result);
	//! Deprecated: Gets a copy of the ST instance text set
	bool getGeometry(TeTextSet& result);
	//! Deprecated: Gets a copy of the ST instance geometry vector
	bool getGeometry(vector<TeGeometry*>& result);
	//! Deprecated: Gets a copy of the ST instance multi geometry
	bool getGeometry(TeMultiGeometry& result);

	//! Deprecated: Get a reference to the ST instance polygon geometry
	TePolygonSet&  getPolygons();
	//! Deprecated: Get a reference to the ST instance line geometry
	TeLineSet&  getLines();
	//! Deprecated: Get a reference to the ST instance point geometry 
	TePointSet&  getPoints();
	//! Deprecated: Get a reference to the ST instance cell geometry  
	TeCellSet&  getCells();
	//! Deprecated: Get a reference to the ST instance text geometry 
	TeTextSet&  getTexts();

	//! Deprecated: Sets a polygon set to the instance 
	void setGeometry(const TePolygonSet& result); 
	//! Deprecated: Sets a line set to the instance 
	void setGeometry(const TeLineSet& result);
	//! Deprecated: Sets a point set to the instance 
	void setGeometry(const TePointSet& result);
	//! Deprecated: Sets a cell set to the instance 
	void setGeometry(const TeCellSet& result);
	//! Deprecated: Sets a text set to the instance 
	void setGeometry(const TeTextSet& result);
	//! Deprecated: Sets a text set to the instance 
	void setGeometry(const TeMultiGeometry& result);
			
	//! Deprecated: Adds a polygon to the instance
	bool addGeometry(const TePolygon& poly);  
	//! Deprecated: Adds a line to the instance
	bool addGeometry(const TeLine2D& line);
	//! Deprecated: Adds a point to the instance
	bool addGeometry(const TePoint& point);	
	//! Deprecated: Adds a cell to the instance
	bool addGeometry(const TeCell& cell);		
	//! Deprecated: Adds a text to the instance
	bool addGeometry(const TeText& cell);		

	//! Returns a centroid of a geometry representation 
	virtual void centroid(TeCoord2D& centroid, TeGeomRep geomRep=TeGEOMETRYNONE); 
	
	//! Returns an area of a geometry representation
	virtual void area(double& a, TeGeomRep geomRep=TeGEOMETRYNONE);

	//! Deprecated: Returns the valid time 
	virtual TeTimeInterval timeInterval () 
	{	return getTime();	}
	
	//! Deprecated: Sets the valid time interval
	virtual void timeInterval (const TeTimeInterval& t) 
	{	setTime(t);	}

	//! Returns the initial time (as a string) of the valid time interval
	virtual string getInitialDateTime(const string& mask="YYYYsMMsDDsHHsmmsSS") 
	{	return time_.getInitialDateTime(mask);	}
	
	//! Returns the final time (as a string) of the valid time interval
	virtual string getFinalDateTime(const string& mask="YYYYsMMsDDsHHsmmsSS") 
	{	return time_.getFinalDateTime(mask);	}
	
};

//! A spatial temporal element or object (TeSTElement) is composite of a set of spatial temporal instances related to it.  
typedef vector<TeSTInstance>  TeSTElement;

#endif 
