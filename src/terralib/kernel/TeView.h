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
/*! \file TeView.h
    \brief This file provides TerraLib's definition of a View
*/
#ifndef  __TERRALIB_INTERNAL_VIEW_H
#define  __TERRALIB_INTERNAL_VIEW_H

#include "TeLayer.h"
#include "TeTheme.h"
#include "TeProjection.h"
#include "TeViewNode.h"
#include <algorithm>

using namespace std;

//! A class to deal with views in TerraLib
/*!
	A View is a structure that aggregated themes to be visualized or
	processed in its own projection. Views may belong to a particular user.
	\sa TeTheme TeViewNode
*/
class TL_DLL TeView
{
public:
	//! Constructor
    TeView( const string& name="", const string& user="", int id=0)
		: id_(id), name_(name), user_(user), proj_(0), is_visible_(true), connectedId_(0), currentTheme_(-1)
	{}

	//! Destructor
	virtual ~TeView () 
	{
		if(proj_)
			delete proj_;
	}

	//! Returns view unique id
	virtual int	id () { return id_; }

	//! Sets view unique id
	virtual void id (int id)
	{
		id_ = id;	// update view id
		for (unsigned int th=0;th<viewTree_.size();th++)	//update its themes
			viewTree_.retrieve(th)->view (id_);
		viewTree_.view(id); 
	}

	//! Returns the view name
	virtual string	name () { return name_; }

	//! Sets the view name
	virtual void name (const string& s) { name_ = s; }

	//! Returns the view user name
	virtual string	user () { return user_; }

	//! Sets the view user name
	virtual void user (const string& i) { user_ = i; }

	//! Returns the view projection
	virtual TeProjection* projection () { return proj_; }

	//! Sets the view projection
	virtual void projection (TeProjection* p) { proj_ = p; }

	//! Returns TRUE if view is visible
	virtual bool isVisible () { return is_visible_; }

	//! Sets whether view is visible
	virtual void isVisible (bool v) { is_visible_ = v; }

	//! Returns the current box
	virtual TeBox& getCurrentBox () { return currentBox_; }

	//! Sets the current box
	virtual void setCurrentBox (const TeBox& b) { currentBox_ = b; }

	//! Returns the current theme id
	virtual int getCurrentTheme () { return currentTheme_; }

	//! Sets the current theme id
	virtual void setCurrentTheme (const int& id) { currentTheme_ = id; }
	
	//! Returns the view tree where view belongs
	virtual TeViewTree* root () { return &viewTree_; }

	//! Adds a view node to the view tree
	virtual void add( TeViewNode* node )
	{
		node->view (id_);
		viewTree_.add ( node );
	}

	//! Removes a view node through its name from the view tree
	virtual TeViewNode* remove(string name)
	{
		return viewTree_.remove(name);
	}

	//! Removes a view node through its identifier from the view tree
	virtual TeViewNode* remove (int id)
	{
		return viewTree_.removeID(id);
	}

	//! Moves a node up
	virtual void moveUp (TeViewNode* node)
	{
		viewTree_.moveUp(node);
	}

	//! Moves a node down
	virtual void moveDown(TeViewNode* node)
	{
		viewTree_.moveDown(node);
	}

	//! Returns a node identified by its position
	virtual TeViewNode* get(int i)
	{
		return viewTree_.retrieve(i);
	}

	//! Returns a node identified by its name and version
	virtual TeTheme* get(string themeName)
	{
		TeTheme *ret = 0;
		for (unsigned int th = 0;th < viewTree_.size();th++)
		{
			TeViewNode *node = viewTree_.retrieve(th);
			if (node->type() == TeTHEME)
			{
				TeTheme *tmp = (TeTheme*)node;
				if (tmp->name() == themeName)
				{
					ret = tmp;
					break;
				}
			}
		}
		return ret;
	}
	
	//! Sort the themes in the view
	virtual void sort() 
	{ viewTree_.sort(); }

	//! Swap the order of two themes
	virtual void swap( unsigned int i, unsigned int j)
	{
		viewTree_.swap(i,j);
	}

	//! Sets the themes visibility 
	virtual void visibility( int vis )
	{
		viewTree_.visibility( vis );
	}

	//! Returns the themes visibility 
	virtual int visibility()
	{
		return viewTree_.visibility();
	}

	//! Returns the number of themes in the view
	virtual unsigned int size()
		{ return viewTree_.size() ; }

	//! Returns the vector of themes of the view sorted according to their priorities
	virtual vector<TeViewNode*>& themes()
	{ return viewTree_.nodes();}

	//! Sets the id of the connected view
	virtual void connectedId(int id) {connectedId_ = id;}

	//! Returns the id of the connected view
	virtual int connectedId() {return connectedId_;}

	/*! Returns the boundary box of the view themes
		\param onlyVisible If is true, returns the boundary box of visible themes, otherwise returns the boundary box of all themes
	*/
	virtual TeBox box( bool onlyVisible )
	{ 
		TeBox retval;
		if ( root() ) 
			retval = root()->box(onlyVisible,proj_); 
		return retval;		
	}; 

private:
	int				id_;	// view id
	string			name_;	// view name
	string			user_;	// user id
	TeProjection*   proj_;
	bool			is_visible_;

	TeViewTree		viewTree_;
	int				connectedId_; // connected view

	TeBox			currentBox_; // current box
	int				currentTheme_; // current theme id
};

//! A map from identifiers to pointers to views
typedef map<int,TeView*> TeViewMap;

#endif



