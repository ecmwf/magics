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
/*! \file TeQuerierParams.h
	\brief This file contains a class responsible for defining the querier mechanism behavior. 
*/

#ifndef  __TERRALIB_INTERNAL_QUERIER_PARAMS_H
#define  __TERRALIB_INTERNAL_QUERIER_PARAMS_H

#include "TeTheme.h"

/*! \class TeQuerierParams
	\brief A class responsible for defining the querier mechanism behavior.

	This class contains a set of attributes that define the querier behavior, 
	for example, which instances the querier will load or if the querier will load
	the geometries and which attributes.
	It is used in the querier constructor. 

	\sa
	TeQuerier
*/
class TL_DLL TeQuerierParams
{
protected:	
	bool					loadGeometries_;		//!< if the querier must load the geometries of each instance
	bool					loadAllAttributes_;		//!< if the querier must load all attributes of each instance
	vector<string>			loadAttrNames_;			//!< which attributes the querier must load
	vector<string>			loadAttrNamesSeted_;	//!< internal structure
	TeGroupingAttr			groupingAttr_;			//!< which attributes will be load and which aggregate function will be used in each attribute 
	
	bool					themeFlag_;		//!< if the theme is temporary
	TeTheme*				theme_;			//!< pointer to a source theme 
	TeLayer*				layer_;			//!< pointer to a source layer 
	
	string					fileName_;		//!< source file name (.shp, etc)
	TeBox					box_;			//!< minimal bounding box that contains all geometries returned by this querier 

	TeChronon				chr_;		//!< chronon used to generate time frames 
	string					objId_;		//!< object identification
	
	//TeStatisticType		tsOper_;		//operator used in the construction of temporal series
	
	string					strategy_;		//! querier strategy name

	TeSelectedObjects		selectedObjs_;  //!< if the querier must load only the instances pointed or queried
	
	int						spatialRelation_;	//!< spatial relation used by spatial restriction
	bool					hasSpatialRes_;		//!< Verifies if there is a spatial restriction defined 
	TeBox					boxRest_;			//!< A bounding box used by spatial restriction 
	TeGeometry*				geomRest_;			//!< A geometry pointer used by spatial restriction 
	TeGeomRep				geomRepRest_;		//!< geometric representation of the theme that will be considered by spatial restriction  

public:
	//! Constructor
	TeQuerierParams(bool loadGeom = false, bool loadAllAttr = true): 
			loadGeometries_(loadGeom),
			loadAllAttributes_(loadAllAttr),
			themeFlag_(false),
			theme_ (0),
			layer_ (0),
			fileName_(""),
			box_(TeBox()),
			chr_ (TeNOCHRONON),
			objId_(""),
			selectedObjs_(TeAll),
			spatialRelation_(0),
			hasSpatialRes_(false),
			geomRest_(0)
			{ }

	//! Constructor
	TeQuerierParams(bool loadGeom, const vector<string>& loadAttr): 
			loadGeometries_(loadGeom),
			loadAllAttributes_(false),
			themeFlag_(false),
			theme_ (0),
			layer_ (0),
			fileName_(""),
			box_(TeBox()),
			chr_ (TeNOCHRONON),
			objId_(""),
			selectedObjs_(TeAll),
			spatialRelation_(0),
			hasSpatialRes_(false),
			geomRest_(0)
			{
				loadAttrNamesSeted_.clear();
				loadAttrNamesSeted_ = loadAttr;
			}


	//! Constructor
	TeQuerierParams(bool loadGeom, TeGroupingAttr& groupAttr): 
			loadGeometries_(loadGeom),
			loadAllAttributes_(false),
			themeFlag_(false),
			theme_ (0),
			layer_ (0),
			fileName_(""),
			box_(TeBox()),
			chr_ (TeNOCHRONON),
			objId_(""),
			selectedObjs_(TeAll),
			spatialRelation_(0),
			hasSpatialRes_(false),
			geomRest_(0)
			{
				groupingAttr_.clear();
				groupingAttr_ = groupAttr;
			}
	
	//! Destructor
	virtual ~TeQuerierParams();

	//! Copy Constructor
	TeQuerierParams(const TeQuerierParams& qp);

	//! Assignment operator
	virtual TeQuerierParams& operator=(const TeQuerierParams& rhs);

	//! Sets the layer that will be used as source of instances 
	virtual void setParams(TeLayer* layer);

	//! Sets the theme that will be used as source of instances and a chronon that defines the time frames
	virtual void setParams(TeTheme* theme, TeChronon chr=TeNOCHRONON); 

	//! Sets the theme that will be used as source of instances of a specific object and a chronon that defines the time frames
	virtual void setParams(TeTheme* theme, const string& objId, TeChronon chr=TeNOCHRONON);

	//! Sets the file name that will be used as source of instances and a chronon that defines the time frames
	virtual void setParams(const string& fileName, TeChronon chr=TeNOCHRONON);
	
	//! Sets the parameters used to fill the instances 
	virtual void setFillParams(bool loadGeom, bool loadAllAttr, vector<string> loadAttr = vector<string>());

	//! Sets the parameters used to fill the instances
	virtual void setFillParams(bool loadGeom, TeGroupingAttr attrG);
	
	//! Sets a spatial restriction (a spatial relation and a bounding box) 
	virtual void setSpatialRest(TeBox& box, int relation = TeWITHIN, TeGeomRep rep = TeGEOMETRYNONE); 

	//! Sets a spatial restriction (a spatial relation and a geometry) 
	virtual void setSpatialRest(TeGeometry* geom, int relation = TeWITHIN, TeGeomRep rep = TeGEOMETRYNONE); 

	//! Sets the selected objects
	virtual void setSelecetObjs(TeSelectedObjects so) { selectedObjs_ = so; }

	//! Sets which attributes must be loaded
	virtual void setLoadAttrs(const vector<string>& vec) { loadAttrNames_ = vec; }

	//! Returns the identification of the strategy associated to the querier
	virtual string decName() const { return strategy_; }

	//! Returns the source theme
	virtual TeTheme* theme() { return theme_; }

	//! Returns the source layer
	virtual TeLayer* layer() { return layer_; }

	//! Returns the source file name
	virtual string fileName() { return fileName_; }

	//! Returns the minimal bounding box
	virtual TeBox& box();

	//! Sets the minimal bounding box 
	virtual void box(TeBox& b) { box_ = b; }

	//! Returns the chronon used to create the time frames
	virtual TeChronon chronon()	{ return chr_; }
	
	//! Returns the object identification
	virtual string objId() { return objId_; }
	
	//! Returns which attributes will be loaded and how they will be grouped
	virtual TeGroupingAttr& groupAttr() { return groupingAttr_;}

	//! Returns if the querier must load the geometries
	virtual bool loadGeom ()  { return	loadGeometries_; }
	
	//! Returns if the querier must load all attributes
	virtual bool loadAllAttr() { return loadAllAttributes_;}

	//! Returns the subset of attributes that will be loaded
	virtual vector<string>& loadAttrs() { return loadAttrNames_;}

	//! Internal function. Returns the subset of attributes that will be loaded
	virtual vector<string>& loadSetedAttrs() { return loadAttrNamesSeted_;}

	//! Returns which objects must be loaded
	virtual TeSelectedObjects selectedObjs() { return selectedObjs_; }
	
	//! Returns the spatial relation 
	virtual int spatialRelation() { return spatialRelation_; }

	//! Verifies if there is a spatial restriction defined
	virtual bool hasSpatialRes() { return hasSpatialRes_;}
	
	//! Returns the bounding box associated to the spatial restriction
	virtual TeBox boxRest() { return boxRest_; } 

	//! Returns the geometry associated to the spatial restriction
	virtual TeGeometry* geomRest() { return geomRest_; }

	//! Returns which theme geometric representation must be considered by the spatial restriction  
	virtual TeGeomRep  geomRepRest() { return geomRepRest_; }

	//! Clear querier params
	virtual void clear();
};

#endif
