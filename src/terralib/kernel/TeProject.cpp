/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2006 INPE and Tecgraf/PUC-Rio.

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
#include "TeProject.h"
#include "TeDatabase.h"

TeProject::TeProject():
	id_(-1),
	name_(""),
	description_(""), 
	currentViewIndex_(-1),
	db_(0)
{}

//! Constructor from a identifier
TeProject::TeProject(int id, TeDatabase* db):
	id_(id),
	name_(""),
	description_(""), 
	currentViewIndex_(-1),
	db_(db)
{
	if (!db)
		return;
	string sql  = "SELECT name, decription, current_view, view_id ";
	       sql += "FROM te_project INNER JOIN te_project_view ON te_project.project_id = te_project_view.project_id";
		   sql += " WHERE te_project.project_id = " + Te2String(id);
	sql += Te2String(id);
	TeDatabasePortal* portal = db->getPortal();
	if (!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return;
	}
	// use the first fetch to get basic information
	name_ = portal->getData(0);
	description_ = portal->getData(1);
	int currentview = portal->getInt(2);
	int viewid = portal->getInt(3);
	views_.push_back(portal->getInt(3));
	if (viewid == currentview)
		currentViewIndex_ = 0;

	// get the other views
	while (portal->fetchRow())
	{
		viewid = portal->getInt(3);
		views_.push_back(viewid);
		if (viewid == currentview)
			currentViewIndex_ = views_.size()-1;
	}
	delete portal;
}

	//! Constructor from a name
TeProject::TeProject(const string& name, TeDatabase* db):
	id_(-1),
	name_(name),
	description_(""), 
	currentViewIndex_(-1),
	db_(db)
{
	if (!db)
		return;
	string sql  = "SELECT te_project.project_id, decription, current_view, view_id ";
	       sql += "FROM te_project INNER JOIN te_project_view ON te_project.project_id = te_project_view.project_id";
		   sql += " WHERE name = '" + name + "'";

	TeDatabasePortal* portal = db->getPortal();
	if (!portal->query(sql) || !portal->fetchRow())
	{
		delete portal;
		return;
	}
	// use the first fetch to get basic information
	id_ = portal->getInt(0);
	description_ = portal->getData(1);
	int currentview = portal->getInt(2);
	int viewid = portal->getInt(3);
	views_.push_back(portal->getInt(3));
	if (viewid == currentview)
		currentViewIndex_ = 0;

	// get the other views
	while (portal->fetchRow())
	{
		viewid = portal->getInt(3);
		views_.push_back(viewid);
		if (viewid == currentview)
			currentViewIndex_ = views_.size()-1;
	}
	delete portal;
}

TeProject::TeProject(const TeProject& other)
{
	id_ = other.id_;
	name_= other.name_;
	description_= other.description_; 
	views_= other.views_;
	currentViewIndex_= other.currentViewIndex_;
	db_= other.db_;
}

TeProject& 
TeProject::operator= (const TeProject& other)
{
	if ( this != &other )
	{
		id_ = other.id_;
		name_= other.name_;
		description_= other.description_; 
		views_= other.views_;
		currentViewIndex_= other.currentViewIndex_;
		db_= other.db_;
	}
	return *this;
}

//! Destructor
TeProject::~TeProject()
{
	views_.clear();
	db_=0;
}

	//! Returns TRUE if a view is part of a project and FALSE otherwise
bool 
TeProject::isProjectView(int viewId)
{
	TeViewVector::iterator it = find(views_.begin(), views_.end(), viewId);
	return (it != views_.end());
}

	//! Returns TRUE if a project has a current view and FALSE otherwise
bool
TeProject::hasCurrentView()
{	return (currentViewIndex_>=0 && currentViewIndex_< static_cast<int>(views_.size())); }


//! Returns the id of the current view in the project 
int
TeProject::getCurrentViewId()
{
	if (currentViewIndex_>=0 && currentViewIndex_< static_cast<int>(views_.size()))
		return views_[currentViewIndex_];
	else
		return -1;
}

//! Sets the id of the current view in the project 
void 
TeProject::setCurrentViewId(int viewId)
{
	currentViewIndex_ = -1;
	for (unsigned int i=0; i<views_.size(); ++i)
	{
		if (views_[i] == viewId)
		{
			currentViewIndex_ = i;
			break;
		}
	}
}

	//! Returns the id of the index-th view of the project
int 
TeProject::getViewId(int index)
{
	if (index >= 0 && index < static_cast<int>(views_.size()))
		return views_[index];
	else
		return -1;
}

//!	Returns the names of the views in the project.
TeViewNameVector
TeProject::getViewNameVector()
{
	TeViewNameVector viewNames;
	if (!db_ || views_.empty())
		return viewNames;

	string viewids = Te2String(views_[0]);
	for (unsigned int i=1; i<views_.size(); ++i)
	{
		viewids += ",";
		viewids += Te2String(views_[i]);
	}
	string sql  = "SELECT te_view.name ";
	       sql += "FROM te_view INNER JOIN te_project_view ON te_view.view_id = te_project_view.view_id ";
	       sql += "WHERE te_project_view.project_id = " + Te2String(id_);

	TeDatabasePortal* portal = db_->getPortal();
	if (!portal)
		return viewNames;
	if (!portal->query(sql))
	{
		delete portal;
		return viewNames;
	}
	while (portal->fetchRow())
		viewNames.push_back(portal->getData(0));
	delete portal;
	return viewNames;
}

int 
TeProject::insertView(string name, TeProjection* projection)
{
	TeProjection* proj;
	if (!db_)
		return -1;
	if (projection)
		proj = projection;
	else
	{
		TeDatum sad69 = TeDatumFactory::make("SAD69");
		proj = new TeLatLong(sad69);
	}
	TeView* view = new TeView(name);
	view->projection(proj);
	view->user(db_->user());
	if (db_->insertView(view))
	{
		return insertViewRel(view->id());
	}
	return -1;
}

int 
TeProject::insertViewRel(int viewId)
{
	if (!db_)
		return -1;

	if(db_->insertProjectViewRel(id_,viewId))
		return addView(viewId);

	return -1;
}

int 
TeProject::addView(int viewId)
{
	if (viewId <=0)
		return -1;
	views_.push_back(viewId);
	currentViewIndex_ = views_.size()-1;
	return viewId;
}

void 
TeProject::deleteView(int viewId)
{
	if (!db_)
		return;
	TeViewVector::iterator pos = find(views_.begin(),views_.end(),viewId);
	if (pos != views_.end())
		views_.erase(pos);
	db_->deleteView(viewId);
}

//! Clear the project
void 
TeProject::clearViews()
{
	views_.clear();
	currentViewIndex_ = -1;
}


