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
/*! \file TeQuerierDBStr3.h
	\brief This file contains a mechanism named "TeQuerierDBStr3" that is 
	responsible for loading spatio-temporal elements from a TerraLib database.
*/
#ifndef  __TERRALIB_INTERNAL_QUERIER_DBSTR3_H
#define  __TERRALIB_INTERNAL_QUERIER_DBSTR3_H

#include "TeQuerierDB.h"
#include "TeTemporalSeries.h"

/*! \class TeQuerierDBStr3
	\brief A class responsible for loading spatio-temporal elements from a TerraLib database.

	This class implements a mechanism that is responsible for loading 
	spatio-temporal elements from a TerraLib database, following a specific strategy (Strategy 3). 
	Each spatio-temporal elements is represented through a TeSTInstance class. 
	This class is internally used by the factory of queriers. It should NOT be used 
	by anyone. This Strategy 2 works with simple chronon, with geometry (points or cells), 
	theme with collection table and existence operator in the DBMS.  

	\sa
	TeQuerierDB TeQuerierParams TeSTInstance 
*/

class TL_DLL TeQuerierDBStr3: public TeQuerierDB
{
private:
	int				timeFramePortal_;	// the time frame appointed by the portal 
	TeTSEntry		TSEntry_;
	TeGeomRep		rep_;

	string						fromClause_;   //store the sql clause(from and where) to return the number of instances
	string						whereClause_;
		
	/* @name Internal functions to initialize portals */
	//@{
	bool initPortal(TeTSEntry* ent=0); 
	//@}

	//! Internal function to fill STO
	bool fillSTO(TeSTInstance& sto); 
	
public:	
	//! Constructor
	TeQuerierDBStr3(TeQuerierParams* params): 
		TeQuerierDB(params), 
		timeFramePortal_(-1),
		fromClause_(""),
		whereClause_("")
	{} 

	//! Loads the instances
	bool loadInstances(TeTSEntry* ent=0); 

	//! Returns each loaded STO
	bool fetchInstance(TeSTInstance&  sto); 

	//! Returns the number of instances 
	int  numElemInstances();
};

/*! \class TeQuerierDBStr3Factory
	\brief A class that define a factory to build a querier strategy (strategy 3) from TerraLib database.

	\sa
	TeQuerierImplFactory TeQuerierDBStr3
*/
class TL_DLL TeQuerierDBStr3Factory : public TeQuerierImplFactory
{
public:

	//! Constructor
	TeQuerierDBStr3Factory(const string& name) : TeQuerierImplFactory(name) {}

	//! Builds a database querier
	virtual TeQuerierImpl* build (const TeQuerierParams& arg)
	{  
		TeQuerierParams* tempArg = new TeQuerierParams();
		*tempArg = arg;
		return new TeQuerierDBStr3(tempArg); 
	}
};

//! Creates a static factory to build TeQuerierDBStr3 
namespace 
{
  static TeQuerierDBStr3Factory querierDBStr3("querierDBStr3");
};

#endif
