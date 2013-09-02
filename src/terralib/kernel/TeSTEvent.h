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
/*! \file TeSTEvent.h
	\brief This file contains structures and definitions to deal with spatial 
	temporal events. 
*/

#ifndef  __TERRALIB_INTERNAL_STEVENT_H
#define  __TERRALIB_INTERNAL_STEVENT_H

#include "TeBaseSTInstance.h"
#include "TeBaseSTInstanceSet.h"
#include "TeGeometry.h"
#include "TeKdTree.h"


/*! \class TeSTEvent
	\brief A class that represents spatial temporal events (STEvent).

	A spatial temporal event is represented by a location (point), 
	a time when it happened and a set of attributes. This class 
	specializes the base class TeBaseSTInstance representing the geometries 
	as points (TePoint) and the valid time as a integer (long).

	\sa TeBaseSTInstance TePoint 
*/
class TL_DLL TeSTEvent : public TeBaseSTInstance<TePoint, long>
{	
public:
	
	//! Empty constructor
	TeSTEvent() : TeBaseSTInstance<TePoint, long>()
	{ }

	//! Constructor
	TeSTEvent(const string& objId, const TePoint& point, const long& time) :
		TeBaseSTInstance<TePoint, long>(objId, point, time)
	{ }

	//! Constructor
	TeSTEvent(const string& objId, const TePoint& point, const vector<string>& attrValues, 
		TeAttributeList* attList, const long& time) :
		TeBaseSTInstance<TePoint, long>(objId, attrValues, attList, point, -1, time)
	{ }

	//! Clear
	virtual void clear(); 
	
	//! Verifies if the time associated to this event is valid
	virtual bool isTimeValid(); 

	//! Deprecated: Gets the event location (point)
	void getGeometry(TePoint& result)
	{	result = this->geometries(); }

	//! Deprecated: Returns the valid time 
	virtual TeTimeInterval timeInterval () 
	{	TeTimeInterval t; return t;	}
};


/*! \class TeSTEventSet
	\brief A class that represents a set of spatial temporal events.

	This class specializes the abstract class TeBaseSTInstanceSet representing
	each spatial temporal instance of the set as a TeSTEvent type. That is, the 
	geometries of each instance or event in this set is represented as a point 
	(TePoint class)and its valid time as a simple number (long type). 

	\sa TeBaseSTInstanceSet TeSTEvent TePoint TeTheme TeLayer
*/
class TL_DLL TeSTEventSet : public TeBaseSTInstanceSet<TePoint, long, TeSTEvent>
{
protected:

	typedef TeSAM::TeAdaptativeKdTreeNode<TeCoord2D, vector<TePoint>, TePoint> kdNode;
	typedef TeSAM::TeAdaptativeKdTree<kdNode> kdTree;

	//! Index structure (KD Tree) to index the point locations
	kdTree*		kdTree_;

	//! Build the set using querier
	bool buildImpl(TeQuerier* querier, const int& slide = -1);

public:			
	//! Constructor	
	TeSTEventSet(TeTheme* theme=0, const TeAttributeList& attrList = TeAttributeList()); 

	//! Destructor	
	~TeSTEventSet();

	//! Build a index structure (kdTree) over all event points in this set
	virtual bool buildKdTree(const int& n); 
	
	//! Searchs the event points inside a specific bounding box 
	virtual bool search(const TeBox& b, vector<TeSTEvent* >& result);

	//! Searchs the k-nearest neighbours of a specific coordinate
	virtual bool nearestNeighbourSearch(const TeCoord2D& coord, vector<TeSTEvent* >& result, vector<double>& distances, const unsigned int& k);
	
	//! Clears the event set 
	virtual void clear(); 
};

#endif







