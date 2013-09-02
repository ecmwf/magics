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
	\brief This file deals an abstract querier class to build STOs (Spatial Temporal Objects)
*/
#ifndef  __TERRALIB_INTERNAL_QUERIER_IMPL_H
#define  __TERRALIB_INTERNAL_QUERIER_IMPL_H

#include "TeQuerierParams.h"
#include "TeFactory.h" 
#include "TeSharedPtr.h" 

class TeTSEntry;
class TeTSParams;
class TeSTInstance;
class TeTemporalSeries;
class TeMultiGeometry;

//! An abstract querier class to build STOs (Spatial Temporal Objects)
class TL_DLL TeQuerierImpl
{
protected:
	//! querier parameters that define the used querier strategy  
	TeQuerierParams*				params_;
	TeTemporalSeries*				ts_;			//! temporal series
	bool							filledSerie_;	//! this flag indicates if the temporal serie was filled 
	TeSharedPtr<TeAttributeList>	attrList_;		//! list of the attributes of the querier 
		
public:
	//! Constructor
	TeQuerierImpl(TeQuerierParams* par); 

	//! Copy constructor
	TeQuerierImpl( const TeQuerierImpl& other);

	//! Operator =
	TeQuerierImpl& operator= (const TeQuerierImpl& other);

	//! Destructor
	virtual ~TeQuerierImpl(); 

	//! Returns the object identifier
	string objectId() { return params_->objId();}

	//! Returns the theme pointer
	TeTheme* theme() { return params_->theme();} 

	//! Loads the STOs
	bool loadTimeFrameInstances(int frame=-1); 

	//! Gets each temporal serie entry, for each time frame
	bool getTSEntry(TeTSEntry& tsEntry, int frame);
	
	//! Gets the number of generated time frames 
	int getNumTimeFrames(); 

	//! Gets the full temporal serie
	bool getTS(TeTemporalSeries& ts);

	//! Returns the temporal serie params
	TeTSParams& getTSParams();

	//! Gets the attribute list from querier
	TeAttributeList getAttrList() { return (*attrList_); }

	//! Returns a default object
	static TeQuerierImpl* DefaultObject(const TeQuerierParams& /*params*/)  { return 0; }

	//! Returns the querier parameters
	TeQuerierParams* params() { return params_; }
	
	//! Loads the STOs
	virtual bool loadInstances(TeTSEntry* ent=0) = 0; 
	
	//! Returns each loaded STO
	virtual bool fetchInstance(TeSTInstance& stoi)= 0;  

	//! Loads all geometries of the index-th geometry representation  
	virtual bool loadGeometries(TeMultiGeometry& geometries, unsigned int& index) = 0; 

	//! Loads all geometries 
	virtual bool loadGeometries(TeMultiGeometry& geometries) = 0;

	//! Returns the number of instances to each time frame
	virtual int numElemInstances() { return 0;}

	//! Empties querier instances
	virtual void clear() { return; }
};

//! A class that define a factory to build querier strategies 
class TL_DLL TeQuerierImplFactory : public TeFactory<TeQuerierImpl, TeQuerierParams>
{
public:
	//! Builds an appropriate decoder from a identifier
	TeQuerierImplFactory(const string& name) : TeFactory<TeQuerierImpl, TeQuerierParams>(name) { }

	//! Destructor
	virtual ~TeQuerierImplFactory() {}
};

#endif
