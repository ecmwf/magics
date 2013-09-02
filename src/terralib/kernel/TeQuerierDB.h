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
/*! \file TeQuerierDB.h
	\brief This file deals with strategies to retrieve spatio-temporal elements from a TerraLib database
*/
#ifndef  __TERRALIB_INTERNAL_QUERIER_DB_H
#define  __TERRALIB_INTERNAL_QUERIER_DB_H

#include "TeQuerierImpl.h"
#include "TeRepresentation.h"

class TeDatabasePortal;
class TeTimeInterval;
class TeTSEntry;

//! auxiliary functions to add geometries in the TeSTInstance or TeMultiGeometry
TL_DLL bool addGeometry(TeDatabasePortal* portal, TeGeomRep geomRep, TeSTInstance& sto, const int& linkIndex, const int& gIdIndex);
TL_DLL bool addGeometry(TeDatabasePortal* portal, TeGeomRep geomRep, TeSTInstance& sto, const int& linkIndex, 
			const int& geomIdIndex, TeTimeInterval time, const int& initTimeIndex, const int& finalTimeIndex); 
TL_DLL bool addGeometry(TeDatabasePortal* portal, TeGeomRep geomRep, TeMultiGeometry& geometries); 


//! An abstract class to build STOs (Spatial Temporal Objects) from a TerraLib database 
class TL_DLL TeQuerierDB : public TeQuerierImpl
{
protected:
	//! a portal to each geometry representation
	vector<TeDatabasePortal*>	portals_;		
	//! keep the geometry representation of each portal 
	vector<TeRepresentation>	geomRepr_;	
	//! internal information used to fill the stos
	TeTable						attrTable_;	
	//! flag to control the portal
	bool						flagPortal_;	 
	
	//! map FROM each portal in the vector "portals_" TO the indexes of the unique attributes
	map<int, vector<int> >		uniqueIndex_; 
	
	//! index of the link attributes in each portal in the vector "portals_"
	vector<int>		linkIndex_;

	//! index in the first portal of the first attribute
	int				attrIndex1_;
	//! index in the first portal of the last attribute
	int				attrIndex2_;
	//! index in the first portal of the group, if there is collection table
	int				groupIndex_;
	//! index where the geometry information begin in each portal in the vector "portals_"
	vector<int>				geomIndex1_;
	//! index where the geometry information finish in each portal in the vector "portals_"
	vector<int>				geomIndex2_;
	//! index of the initial time in each portal in the vector "portals_"  
	vector<int>				timeIndex1_;
	//! index of the final time in each portal in the vector "portals_"  
	vector<int>				timeIndex2_;

	//! map from legend identifier to group number  
	map<int, int>			legendIdGroup_;
			
	//! clear internal vectors
	void clearVectors(); 

	//! Return a where clause in SQL that represent the querier restrictions
	virtual string sqlWhereRestrictions(TeRepresentation* rep=0);

	//! Build the sql from clause
	string sqlFrom(string geomTable="");

public:	
	//! Constructor
	TeQuerierDB(TeQuerierParams* params): TeQuerierImpl(params), flagPortal_(false) 
	{}
	
	//! Destructor 
	virtual ~TeQuerierDB();  
	
	//! Returns the pointer to the theme 
	TeTheme* theme() {	return params_->theme();	}

	//! Returns the pointer to the layer 
	TeLayer* layer() {	return  params_->theme()->layer(); }

	//! Returns the geometry representation of the theme
	TeGeomRep geometryRep();

	//! Loads the STOs from database
	virtual bool loadInstances(TeTSEntry* ent=0) = 0;  
	
	//! Returns each loaded STO
	virtual bool fetchInstance(TeSTInstance& stoi) = 0; 

	//! Loads all geometries of the index-th geometry representation  
	virtual bool loadGeometries(TeMultiGeometry& geometries, unsigned int& index); 

	//! Loads all geometries 
	virtual bool loadGeometries(TeMultiGeometry& geometries);
	
	//! Empties querier instances
	void clear(); 
};

#endif
