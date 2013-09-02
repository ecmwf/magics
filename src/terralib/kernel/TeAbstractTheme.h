/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright � 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeAbstractTheme.h
	\brief This file contains definitions about an abstract theme in TerraLib. 
*/
#ifndef  __TERRALIB_INTERNAL_ABSTRACT_THEME_H
#define  __TERRALIB_INTERNAL_ABSTRACT_THEME_H

#include "TeDefines.h"
#include "TeLegendEntry.h"
#include "TeGeometry.h"
#include "TeViewNode.h"
#include "TeAttribute.h"

class TeRasterTransform;
class TeRaster;
class TeDatabase;
struct TeRepresentation;

#include <string>
#include <set>

//! A grouping structure
/*!
	The TeGrouping is a structure that describes how the objects of a theme should
	be grouped, or separated in groups.
*/
struct TL_DLL TeGrouping
{
	//! Returns the attribute used to group the objects of the theme
	TeAttributeRep	groupAttribute_;
	
	//! Returns the attribute used to normalize a grouping 
	string	groupNormAttribute_;
	
	//! Returns the grouping mode 
	TeGroupingMode	groupMode_;			
	
	//! Returns the number of groups
	int	groupNumSlices_;	
	
	//! Returns the numeric precision used to group objects
	int	groupPrecision_;

	//! Returns the deviation parameter used to group objects
	double	groupStdDev_;

	//! Aggregate function
	string	groupFunction_; 

	//! Show missing data
	bool	groupNullAttr_; 

	//! Minimum value used to calculate a equal step grouping
	double groupMinVal_;

	//! Maximum value used to calculate a equal step grouping 
	double groupMaxVal_;

	//! Chronon used for grouping
	TeChronon groupChronon_;

	//! Constructor
	TeGrouping(TeAttributeRep att=TeAttributeRep(), const string& normAttr="", 
				TeGroupingMode gMode=TeNoGrouping, int numSlice=0, int gPrecision=6,  
				double	gStDev=1.0, const string& func="", TeChronon gChronon = TeNOCHRONON): 
			groupAttribute_(att),
			groupNormAttribute_(normAttr),
			groupMode_(gMode),
			groupNumSlices_(numSlice),
			groupPrecision_(gPrecision),
			groupStdDev_(gStDev),
			groupFunction_(func),
			groupNullAttr_(false),
			groupMinVal_(TeMAXFLOAT),
			groupMaxVal_(TeMINFLOAT),
			groupChronon_(gChronon)
			{}

	//! Copy constructor
	TeGrouping(const TeGrouping& other)
	{	
		groupAttribute_		= other.groupAttribute_;
		groupNormAttribute_	= other.groupNormAttribute_;
		groupMode_			= other.groupMode_;
		groupNumSlices_		= other.groupNumSlices_;
		groupPrecision_		= other.groupPrecision_;
		groupStdDev_		= other.groupStdDev_;
		groupFunction_		= other.groupFunction_;
		groupNullAttr_		= other.groupNullAttr_;
		groupMinVal_		= other.groupMinVal_;
		groupMaxVal_		= other.groupMaxVal_;
		groupChronon_			= other.groupChronon_;
	}
			
	//! Destructor
	~TeGrouping() {}
	
	//! Copy constructor
	TeGrouping& operator= (const TeGrouping& other)
	{	
		if ( this != &other )
		{		
			groupAttribute_		= other.groupAttribute_;
			groupNormAttribute_	= other.groupNormAttribute_;
			groupMode_			= other.groupMode_;
			groupNumSlices_		= other.groupNumSlices_;
			groupPrecision_		= other.groupPrecision_;
			groupStdDev_		= other.groupStdDev_;
			groupFunction_		= other.groupFunction_;
			groupNullAttr_		= other.groupNullAttr_;
			groupNullAttr_		= other.groupNullAttr_;
			groupMinVal_		= other.groupMinVal_;
			groupMaxVal_		= other.groupMaxVal_;
			groupChronon_		= other.groupChronon_;
		}
		return *this;
	}
};

using namespace std;


//! An abstract theme class
class TL_DLL TeAbstractTheme: public TeViewNode
{

	friend class TeDatabase;
	
public:
	//! Constructor
    TeAbstractTheme(const string& name, TeViewNode* parent, const int& view, const int& id, const TeViewNodeType& nodeType);

	//! Constructor
	TeAbstractTheme(const TeViewNodeParams& params);

	//! Copy constructor
	TeAbstractTheme(const TeAbstractTheme& other);

	//! Destructor
	virtual ~TeAbstractTheme();

	//! Assignment operator
	TeAbstractTheme& operator= (const TeAbstractTheme& other); 

	/** @name Scale
		Methods related to the range of scales where this theme should be visible
	*/
	//@{	

	//! Returns a pointer to a projection that is the spatial reference for the objects of this theme
	/*! Concrete classes should reimplement this method.*/
	virtual TeProjection* getThemeProjection() = 0;

	//! Returns the minimum scale in which the theme is visible
	virtual double minScale() { return minScale_; }

	//! Sets the minimum scale in which the theme is visible
	virtual void minScale(double s) { minScale_ = s; }

	//! Returns the maximum scale in which the theme is visible
	virtual double maxScale() { return maxScale_; }

	//! Sets the maximum scale in which the theme is visible
	virtual void maxScale(double s) { maxScale_ = s; }
	//@}

	/** @name Restrictions
		Methods related to the restrictions over the theme used to generate this theme
	*/
	//@{
	//! Verifies if this theme has any kind of restrictions
	virtual bool hasRestriction() { return (!(generateAttributeRest_.empty() || 
									  generateTemporalRest_.empty()) && hasSpatialRes_); }

	//! Returns the attribute restriction (where clause) used to generate the theme  
	virtual string attributeRest() { return generateAttributeRest_; }

	//! Sets the attribute restriction (where clause) used to generate the theme
	virtual void attributeRest(const string& s) { generateAttributeRest_ = s; }

	//! Returns if there is an attribute restriction  
	virtual bool hasAttrRest () { return (!generateAttributeRest_.empty());}

	//! Returns the temporal restriction used to generate the theme
	virtual string temporalRest() { return generateTemporalRest_; }

	//! Sets the temporal restriction used to generate the theme
	virtual void temporalRest(const string& t) { generateTemporalRest_ = t; };

	//! Returns TRUE if there is a temporal restriction defined in the theme 
	virtual bool hasTemporalRest() { return (!generateTemporalRest_.empty());}

	//! Returns the spatial restriction used to generate the theme
	virtual string spatialRest() { return generateSpatialRest_; }

	//! Sets the spatial restriction used to generate the theme
	virtual void spatialRest(const string& s) { generateSpatialRest_ = s; };
	
	//! Returns the spatial relation (e.g WITHIN, COVERED BY, etc.) associate to the spatial restriction
	virtual TeSpatialRelation	spatialRelation() { return spatialRelation_;}

	//! Sets the spatial relation (e.g WITHIN, COVERED BY, etc.) associate to the spatial restriction
	virtual void spatialRelation(TeSpatialRelation s) {spatialRelation_=s;}
	
	//! Returns TRUE if this theme has a spatial restriction  
	virtual bool hasSpatialRest() { return hasSpatialRes_;}

	//! Sets the flag that indicates that there is a spatial restriction 
	virtual void hasSpatialRest(bool a) {hasSpatialRes_ = a;}
	
	//! Returns the box associated with the spatial restriction
	virtual TeBox boxRestriction() { return boxRest_;}

	//! Sets the box associated with the spatial restriction
	virtual void boxRestriction(TeBox& b) {boxRest_ = b;}

	//! Returns a pointer to the geometry associated with the spatial restriction
	virtual TeGeometry* geomRestriction() { return geomRest_;}

	//! Sets a pointer to the geometry associated with the spatial restriction
	virtual void geomRestriction(TeGeometry* g) {geomRest_ = g;}

	//! Returns the geometry representation of the theme which will be considered in the spatial restriction  
	virtual TeGeomRep geomRepRestriction() { return geomRepRest_; }

	//! Sets the geometry representation of the theme which will be considered in the spatial restriction  
	virtual void geomRepRestriction (TeGeomRep& rep) { geomRepRest_ = rep; }

	//! Sets the spatial restriction to be a spatial relation with a box
	virtual void setSpatialRest(TeBox& box, TeGeomRep rep, TeSpatialRelation relation = TeWITHIN); 

	//! Sets the spatial restriction to be a spatial relation with a geometry
	virtual void setSpatialRest(TeGeometry* geom, TeGeomRep rep, TeSpatialRelation relation = TeWITHIN); 
	//@}
	
	
	/** @name Visibility/Status
		Methods related to the visibility/status of the theme and its components: graphs,
		geometrical representations of the objects, grouping.
	*/
	//@{
	//! Sets the components or representations of the theme that are visible
	/*!
		The param rep is a combination of any type of geometrical representation (TeGeomRep) plus:
		- 0x20000000: representing the visibility of the legend
		- 0x40000000: representing the visibility of the groupings
		- 0x80000000: representing the visibility of the pie/chart graphs
	*/
	virtual void visibleRep(int rep) { visibleRep_ = rep; }

	//! Returns the components or representations of the theme that are visible 
	/*!
		The result is a combination of any type of geometrical representation (TeGeomRep) plus:
		- 0x20000000: representing the visibility of the legend
		- 0x40000000: representing the visibility of the groupings
		- 0x80000000: representing the visibility of the pie/chart graphs
	*/
	virtual int visibleRep() { return visibleRep_; }

	//! Returns the geometrical representations of the theme that are visible
	virtual int visibleGeoRep();

	//! Returns a status of a theme
	/*! 
		- Returns 0 if the theme is not visible and not active
		- Returns 1 if the theme is visible and not active
		- Returns 2 if theme is and active and not visible
		- Returns 3 if theme is and visible and active
	 */
	virtual int visibility()
	{	return enableVisibility_; }

	//! Sets whether the theme should be visible and/or active
	/*! 
		- Returns 0 if the theme is not visible and not active
		- Returns 1 if the theme is visible and not active
		- Returns 2 if theme is and active and not visible
		- Returns 3 if theme is and visible and active
	 */
	virtual void visibility(int v)
	{	enableVisibility_ = v; }
	//@}
	
	/** @name Grouping
		Methods related to grouping of objects of the theme. Each group is
		represented by a specific presentation visual called legend. An slice
		represents the range of values of an attribute that characterizes a group.
	*/
	//@{
	//! Returns a grouping associated the theme 
	virtual TeGrouping& grouping() { return grouping_; }
	
	//! Sets a grouping associated to the theme
	virtual void grouping(const TeGrouping& g) 
	{ grouping_ = g; }

	//! Returns the vector of legends of the theme
	virtual TeLegendEntryVector& legend() { return legend_; }

	//! Sets the visual associated to a geometric representation in the n-th group of the theme 
	virtual bool setGroupingVisual(int n, TeVisual* visual, TeGeomRep rep);

	//! Sets the visual of the n-th group of the theme
	virtual bool setGroupingVisual(int n, TeGeomRepVisualMap& vismap);

	//! Clear the existing grouping of objects of this theme
	virtual void resetGrouping ();

	//! Clear the vector of legends
	virtual void cleanLegend(); 

	//! Save grouping parameters in memory passing an arbitrary set of slices
	virtual bool buildGrouping(const TeGrouping& g, vector<TeSlice>& slices);

	//! Returns the slices associated to the grouping of the theme
	virtual TeSliceVector getSlices();
	//@}

	/** @name Legends
		Legends are also used to define presentation characteristics of the objets
		of the theme that aren't grouped, according to some specific characteristics
		(such as being pointed, being queried, among others).
	*/
	//@{	
	//! Sets a default legend of the objects of the theme
	virtual void defaultLegend (TeLegendEntry& leg) 
	{ defaultLegend_ = leg; }

	//! Returns the default legend of the objects of the theme
	virtual TeLegendEntry& defaultLegend () 
	{ return defaultLegend_; }

	//! Sets the visual of the default legend for a specific geometrical representation
	virtual void setVisualDefault (TeVisual* visual, TeGeomRep rep)
	{ defaultLegend_.setVisual(visual, rep); }

	//! Sets a legend for objects of the layer that weren't selected in this theme
	/*! Useful when is necessary to see the theme in the context of the layer */
	virtual void outOfCollectionLegend (TeLegendEntry &leg) 
	{ outOfCollectionLegend_ = leg;}

	//! Returns the legend of the layer objects that are not selected in this layer.
	virtual TeLegendEntry& outOfCollectionLegend () 
	{ return outOfCollectionLegend_; } 
	
	//! Sets the visual of the non-selected objects legend for a specific geometrical representation
	virtual void setVisualOutOfCollection (TeVisual* visual, TeGeomRep rep)
	{ outOfCollectionLegend_.setVisual(visual, rep); }

	//! Sets a legend for objects that have geometries but not descriptive attributes
	/*! Useful when in intermediary situations such as editing */
	virtual void withoutDataConnectionLegend (TeLegendEntry &leg) 
	{ withoutDataConnectionLegend_ = leg; }

	//! Returns the legend of objects that have geometries but not descriptive attributes
	virtual TeLegendEntry& withoutDataConnectionLegend () 
	{ return withoutDataConnectionLegend_; }

	//! Sets the visual of the without-attributes objects legend for a specific geometrical representation
	virtual void setVisualWithoutDataConnection (TeVisual* visual, TeGeomRep rep)
	{ withoutDataConnectionLegend_.setVisual(visual, rep); }
	
	//! Sets a legend for the theme objects selected by pointing
	virtual void pointingLegend (TeLegendEntry &leg) 
	{ pointingLegend_ = leg; }

	//! Returns the legend of the theme objects selected by pointing
	virtual TeLegendEntry& pointingLegend () 
	{ return pointingLegend_; }

	//! Sets the visual of the pointed objects legend for a specific geometrical representation
	virtual void setVisualPointing (TeVisual* visual, TeGeomRep rep)
	{ pointingLegend_.setVisual(visual, rep); }

	//! Sets a legend for the theme objects selected by a query
	virtual void queryLegend (TeLegendEntry &leg) 
	{ queryLegend_ = leg; }

	//! Returns the legend of the theme objects selected by a query
	virtual TeLegendEntry& queryLegend () 
	{ return queryLegend_; }
	
	//! Sets the visual of the queried objects legend for a specific geometrical representation
	virtual void setVisualQuery (TeVisual* visual, TeGeomRep rep)
	{ queryLegend_.setVisual(visual, rep); }

	//! Sets a legend for the theme objects selected by query and pointing
	virtual void queryAndPointingLegend (TeLegendEntry &leg) 
	{ queryAndPointingLegend_ = leg; }

	//! Returns the legend of the theme objects selected by query and pointing
	virtual TeLegendEntry& queryAndPointingLegend () 
	{ return queryAndPointingLegend_; }

    //! Returns the map of legend ids associated to each object
	virtual map<string, int>& getObjLegendMap() 
	{ return objLegendMap_; }

	//! Returns the map of legend ids associated to each object
	virtual map<string, int>& getObjOwnLegendMap() 
	{ return objOwnLegendMap_; }

	//! Sets the visual of the queried and pointed objects legend for a specific geometrical representation
	virtual void setVisualQueryAndPointing (TeVisual* visual, TeGeomRep rep)
	{ queryAndPointingLegend_.setVisual(visual, rep); }

	//! Sets a legend for the theme objects
	virtual void legend(TeLegendEntry& leg); 

	/** @name Raster Visual
	*  Methods to deal with the visual presentation of the raster representations
	*/
	//@{ 
	//! Returns the visual presentation of raster geometry
	virtual TeRasterTransform* rasterVisual() 
	{ return rasterVisual_; }

	//! Sets the visual presentation of raster geometry
	virtual void rasterVisual(TeRasterTransform* r) 
	{ rasterVisual_ = r; } 

	//! Removes the visual presentation of the raster
	virtual void removeRasterVisual();

	//! Creates an appropriate visual presentation to the raster of the theme
	virtual void createRasterVisual(TeRaster* rst=0);
	//@}
	//@}

	/** @name Box
	    Methods related to the bounding box of a theme
	*/
	//@{ 
	//! Returns the theme box 
	virtual TeBox& getThemeBox()
	{	return themeBox_; }

	//! Sets the theme box 
	virtual void setThemeBox(const TeBox& box)
	{	themeBox_ = box; }

	//! Returns the theme box 
	virtual TeBox& box()
	{	return themeBox_; }
	//@}

	//! Sets the parent node. The abstract theme does not have parent node.
	virtual void setParent (TeViewNode* );
	
	//! Returns TRUE if a theme has lower priority than another
	virtual bool operator< (const TeAbstractTheme& r) const
	{	return viewNodeParams_.priority_ < r.viewNodeParams_.priority_; }

	//! Clear the set containing the objects of the theme
	virtual void clearObjectSet()
	{ objectSet_.clear(); }

	//! Return the number of objects of the layer
	virtual int getNumLayerObjects()
	{ return numLayerObjects_; }

	//! Return the map containing the status of the objects
	virtual map<string, int>& getObjStatusMap()
	{ return objStatusMap_; }

	//! Set the status of the objects to the default state
	virtual void clearObjStatus()
	{ objStatusMap_.clear(); }

	//! Set the status of the items to the default state
	virtual void clearItemStatus()
	{ itemStatusMap_.clear(); }

	//! Return the map containing the status of the items
	virtual map<string, int>& getItemStatusMap()
	{ return itemStatusMap_; }

	//! the status
	virtual void setStatus(vector<string>& oidVec, vector<string>& itemVec, int status);

	//! the status for the items that were toggled
	virtual void setStatusForItemsToggled(set<string>& oidSet, vector<string>& itemVec);

	//! Set the status for the objects that were toggled
	virtual void setStatusForObjectToggled(string oid);

	//! Set the status for the new set of objects that were pointed
	virtual void setStatusForNewObjectsPointed(set<string>& oidSet);

	//! Set the status for the additional set of objects that were pointed
	virtual void setStatusForObjectsAddedByPointing(set<string>& oidSet);

	//! Set the status for the new set of items that were pointed
	virtual void setStatusForNewItemsPointed(vector<string>& itemVec);

	//! Set the status for the additional set of items that were pointed
	virtual void setStatusForItemsAddedByPointing(vector<string>& itemVec);

	//! Set the status for the new set of items that were queried
	virtual void setStatusForNewItemsQueried(set<string>& oidSet, vector<string>& uidVec);

	//! Set the status for the additional set of items that were queried
	virtual void setStatusForItemsAddedByQuerying(set<string>& oidSet, vector<string>& uidVec);

	//! Set the status for the set of items that were filtered by a query operation
	virtual void setStatusForItemsFilteredByQuerying(set<string>& oidSet, vector<string>& uidVec);

	//! Remove the pointing color
	virtual void removePointingColor();

	//! Remove the query color
	virtual void removeQueryColor();

	//! Invert the pointing status of the objects
	virtual void invertObjectStatus();

	//! Set the objects to the default status
	virtual void setObjectsToDefaultStatus();

	//! Save the theme parameters
	virtual bool save() = 0;

	//! Save the grouping parameters in memory when there is no chronon
	virtual bool buildGrouping(const TeGrouping& g, TeSelectedObjects selectedObjects = TeAll,
		               vector<double>* dValuesVec = 0) = 0;

	//! Save the grouping parameters in memory when there is chronon
	virtual bool buildGrouping(const TeGrouping& g, TeChronon chr, vector<map<string, string> >& mapObjValVec) = 0;
	
	
	//! Build the grouping and associate each object to its group  
	virtual bool saveGrouping(TeSelectedObjects selectedObjects = TeAll) = 0;

	//! Delete grouping
	virtual bool deleteGrouping() = 0; 

	//! Set the legend id for each object of the theme 
	virtual void setLegendsForObjects() = 0;

	//! Set the own legend id for each object of the theme 
	virtual void setOwnLegendsForObjects() = 0;

	/** @name Locate geometries
	    Returns the geometry(ies) of the theme given coordinate
	*/
	//@{ 	
	virtual bool locatePolygon		(TeCoord2D &pt, TePolygon &polygon, const double& tol = 0.0) = 0;
	virtual bool locatePolygonSet   (TeCoord2D &pt, double tol, TePolygonSet &polygons) = 0;
	virtual bool locateLine		(TeCoord2D &pt, TeLine2D &line, const double& tol = 0.0) = 0;
	virtual bool locatePoint	(TeCoord2D &pt, TePoint &point, const double& tol = 0.0) = 0;
	virtual bool locateCell		(TeCoord2D &pt, TeCell &c, const double& tol = 0.0) = 0;
	//@}

	//! Get the set of objects corresponding to the object selection criteria
	virtual set<string> getObjects(TeSelectedObjects selectedObjects = TeAll) = 0;

	//! Get the set of objects corresponding to the list of items
	virtual set<string> getObjects(const vector<string>& itemVec) = 0;

	//! Get the set of items corresponding to the object selection criteria
	virtual vector<string> getItemVector(TeSelectedObjects selectedObjects) = 0;

	//! Get the set of items corresponding to the set of objects
	virtual vector<string> getItemVector(const set<string>& oidSet) = 0;

	//! Get the number of objects acessible by this theme
	virtual unsigned int getNumberOfObjects()=0; 

	//! Save (insert or update) the theme metadata in the database
	virtual bool saveMetadata(TeDatabase* ) = 0;
	
protected:
	string		generateAttributeRest_;
	string		generateTemporalRest_;
	string		generateSpatialRest_; //future use

	TeSpatialRelation	spatialRelation_;
	bool				hasSpatialRes_;
	TeBox				boxRest_;		//box which defines the spatial restriction 
	TeGeometry*			geomRest_;		//geometry which defines the spatial restriction 
	TeGeomRep			geomRepRest_;	//geometry representation of the theme which will be 
										//considered in the spatial restriction  
	// Display scale
	double	minScale_;
	double	maxScale_;

	//representation visible in the theme
	int visibleRep_;

	//Theme status
	int	enableVisibility_;

	// ----------------- grouping information -----------------
	TeGrouping		grouping_;

	// ----------------- legend information -----------------
	TeLegendEntryVector	legend_;

	// Background Legend
	TeLegendEntry		outOfCollectionLegend_;			//group(-1) 
	TeLegendEntry		withoutDataConnectionLegend_;	//group(-2) 
	TeLegendEntry		defaultLegend_;					//group(-3) 
	TeLegendEntry		pointingLegend_;				//group(-4) 
	TeLegendEntry		queryLegend_;					//group(-5) 
	TeLegendEntry		queryAndPointingLegend_;		//group(-6)

	map<string, int>	objLegendMap_;					// object legend
	map<string, int>	objOwnLegendMap_;				// object own legend
		
	//! visual of raster
	TeRasterTransform*	rasterVisual_;
	
	TeBox	themeBox_;

	//! A set containing the theme objects
	set<string> objectSet_;

	//! Number of objects of the layer
	int numLayerObjects_;

	//! Status of the item (concatenation of the unique names of each theme table)
	map<string, int> itemStatusMap_;

	//! Status of the object
	map<string, int> objStatusMap_;

	//! Load the theme metadata from database
	virtual bool loadMetadata(TeDatabase* ) = 0;

	//! Erase the theme metadata in the database
	virtual bool eraseMetadata(TeDatabase* ) = 0;
};

//! A vector of pointers to theme
typedef vector<TeAbstractTheme*>	TeThemeVector;

//! A map from theme identifiers to pointers to theme
typedef map<int, TeAbstractTheme*>	TeThemeMap;

/*! \example creaTeAbstractTheme.cpp
	Shows how to create themes in TerraLib.
 */

/*! \example themeGrouping.cpp
	Shows how to do a grouping on the objects of a TerraLib theme.
 */

/*! \example rasterSlicing.cpp
	Shows how to  create a legend over a raster data, stored in a layer TerraLib.
 */
#endif

