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
/*! \file TeProject.h
    \brief This file contains structures and definitions to support the concept of a project in TerraLib
*/
#ifndef  __TERRALIB_INTERNAL_PROJECT_H
#define  __TERRALIB_INTERNAL_PROJECT_H

#include "TeDefines.h"
#include <string>
#include <vector>
#include <map>

class TeDatabase;
class TeProjection;
class TeView;

using std::vector;
using std::string;
using std::map;

//! A vector of the identifiers of the maps in the project.
typedef vector<int> TeViewVector;

//! A vector of the names of the views in the project.
typedef vector<string> TeViewNameVector;

//! A vector of the identifiers of the projects.
typedef vector<int> TeProjectIdVector;

/*!
   This class represents a project concept.
   A project is a collection of views kept in a ordered list.
   A project might have the information about the current view.
*/
class TL_DLL TeProject
{
public:

	//! Empty constructor
	TeProject();

    //! Constructor from a identifier
	TeProject(int id, TeDatabase* db);

	//! Constructor from a name
	TeProject(const string& name, TeDatabase* db);

	//! Copy Constructor
	TeProject(const TeProject& other);

	//! Operator =
	TeProject& operator= (const TeProject& other);

	//! Destructor
	virtual ~TeProject();

	//! Sets the project name.
	virtual void setId(int id) 
	{id_ = id;}	
	
	//! Returns the project name.
	virtual int id() 
	{ return id_; }

	//! Sets the project name.
	virtual void setName(const string& name) 
	{name_ = name;}	
	
	//! Returns the project name.
	virtual string name() 
	{ return name_; }

	//! Returns the project description.
	virtual string description() 
	{ return description_; }

	//! Sets the project description
	virtual void setDescription(const string& desc) 
	{ description_ = desc ; }

	//! Sets the project database
	virtual void setDatabase(TeDatabase* db)
	{ db_ = db; }

	//! Gets the project database
	virtual TeDatabase* getDatabase()
	{ return db_; }

	//! Returns TRUE if a view is part of a project and FALSE otherwise
	virtual bool isProjectView(int viewId);

	//! Returns TRUE if a project has a current view and FALSE otherwise
	virtual bool hasCurrentView();

	//! Sets the id of the current view in the project 
	virtual void setCurrentViewId(int viewId);

	//! Returns the id of the current view in the project 
	virtual int getCurrentViewId();

	//! Sets the index in the list of the current view in the project 
	virtual void setCurrentViewIndex(int index)
	{ currentViewIndex_ = index; }

	//! Returns position of the current view in the project
	virtual int getCurrentViewIndex()
	{ return currentViewIndex_; }

	//! Returns a reference to the project list of views
	const TeViewVector& getViewVector() const
	{	return views_; }

	//!	Returns the names of the views in the project.
	virtual TeViewNameVector getViewNameVector();

	//! Returns the id of the index-th view of the project
	virtual int getViewId(int index);

	//!	Creates a new view in the project and persists it in the database
	/* 
		\param name view name
		\param projection view projection. If null uses as default LatLong/SAD69
		\return the id of the new view
	*/
	virtual int insertView(string name, TeProjection* projection = NULL);

	//!	Adds a new view relation with the project and persists it in the database
	/* 
	\param view The view object
	\return the id of the new view
	*/
	virtual int insertViewRel(int viewId);

	//! Adds an view id to the views project vector
	virtual int addView(int viewId);

	//! Removes a view from the project (and also from the database)
	virtual void deleteView(int viewId);

	//! Clear the project
	virtual void clearViews();

private: 
	int id_;
	string name_;
	string description_; 
	TeViewVector views_;
	int currentViewIndex_;
	TeDatabase* db_;
};

//! A map from a integer unique identifier to a pointer to project
typedef map<int, TeProject*>  TeProjectMap;

#endif
