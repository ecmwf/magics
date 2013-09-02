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

/*! \file TeQuerierDBStr1.h
	\brief This file contains a mechanism named "TeQuerierDBStr1" that is 
	responsible for loading spatio-temporal elements from a TerraLib database. 
*/

#ifndef  __TERRALIB_INTERNAL_QUERIER_DBSTR1_H
#define  __TERRALIB_INTERNAL_QUERIER_DBSTR1_H

#include "TeQuerierDB.h"

/*! \class TeQuerierDBStr1
	\brief A class responsible for loading spatio-temporal elements from a TerraLib database.

	This class implements a mechanism that is responsible for loading 
	spatio-temporal elements from a TerraLib database, following a specific strategy. 
	Each spatio-temporal elements is represented through a TeSTInstance class. 
	This class is internally used by the factory of queriers. It should NOT be used 
	by anyone. 

	\sa
	TeQuerierDB TeQuerierParams TeSTInstance 
*/
class TL_DLL TeQuerierDBStr1 : public TeQuerierDB
{
private:		
	vector<string>				uniqueNames_;
	string						lastObjId_;
	int							geomId_;
	bool						isGroup_;			//! if the portal was built with statistics sql funcionts
	bool						groupInMemory_;		//! if it must group the object instances in the memory 
	string						linkName_;
	
	string						fromClause_;   //store the sql clause(from and where) to return the number of instances
	string						whereClause_;
	
	/* @name Internal functions to initialize portals */
	//@{
	bool initPortal(TeRepresentation& rep, TeTSEntry* ent=0);     
	bool initGeomPortal(TeRepresentation& rep, TeTSEntry* ent=0); 
	//@}

	/* @name Internal functions to fill the STOs from portals */
	//@{
	bool fillSTOGrouped(TeSTInstance& sto, bool fetchInstance = true);
	bool fillSTONoGrouped(TeSTInstance& sto);
	bool fillGeomSTO(TeSTInstance& sto, unsigned int index);
	//@}

public:
	//! Constructor
	TeQuerierDBStr1(TeQuerierParams* params) : 
		TeQuerierDB(params), 
		lastObjId_(""),
		isGroup_(false),
		groupInMemory_(false),
		linkName_(""),
		fromClause_(""),
		whereClause_("")
	{}
	
	//! Loads instances
	bool loadInstances(TeTSEntry* ent=0); 
	
	//! Returns each loaded instance
	bool fetchInstance(TeSTInstance&  sto); 

	//! Returns the number of instances 
	int numElemInstances(); 
};

/*! \class TeQuerierDBStr1Factory
	\brief A class that define a factory to build a querier strategy (strategy 1) from TerraLib database.

	\sa
	TeQuerierImplFactory TeQuerierDBStr1
*/
class TL_DLL TeQuerierDBStr1Factory : public TeQuerierImplFactory
{
public:
	//! Constructor
	TeQuerierDBStr1Factory(const string& name) : TeQuerierImplFactory(name) {}

	//! Builds a database querier
	virtual TeQuerierImpl* build (const TeQuerierParams& arg)
	{  
		TeQuerierParams* tempArg = new TeQuerierParams();
		*tempArg = arg;
		return new TeQuerierDBStr1(tempArg); 
	}
};

//! Creates a static factory to build TeQuerierDBStr1 
namespace 
{
  static TeQuerierDBStr1Factory querierDBStr1("querierDBStr1");
};


#endif

