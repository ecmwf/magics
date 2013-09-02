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
/*! \file TeViewNode.h
    \brief This file provides TerraLib's tree structure of views
*/
#ifndef  __TERRALIB_INTERNAL_VIEWNODE_H
#define  __TERRALIB_INTERNAL_VIEWNODE_H

#define THEME_GROUP

#include "TeDefines.h"
#include "TeBox.h"
#include "TeAbstractFactory.h"

#include <iostream>
#include <string>
#include <vector>
using namespace std;

class TeProjection;
class TeViewNode;

//! Type of view node
enum TeViewNodeType
{ TeTHEME=0, TeTREE=1, TeEXTERNALTHEME=2, TeFILETHEME=3 }; 


//!  A class to represent the view node parameters 
/*!  
	 This class contains the main view node parameters and it is used 
	 by the factory responsible for creating view node objects. 
	 
	\sa
     TeViewNode
*/
class TL_DLL TeViewNodeParams
{
public:
	//! Constructor
	TeViewNodeParams(const string& name="", const int& id=0, const int& viewId=0, 
		const int nodeType=0, const int& priority=0, TeViewNode* parentNode=0,
		const int parentId=0):
		name_(name),
		id_(id),
		viewId_(viewId),
		nodeType_(nodeType),
		priority_(priority),
		myParent_(parentNode),
		myParentId_(parentId)
	{ }

	//! Copy constructor
	TeViewNodeParams(const TeViewNodeParams& params)
	{
		name_ = params.name_;
        id_ = params.id_;
		viewId_ = params.viewId_; 
        nodeType_ = params.nodeType_; 
		priority_ = params.priority_;
		myParent_ = params.myParent_;
		myParentId_ = params.myParentId_; 
	}

	//! Returns the view node type 
	int getProductId() { return nodeType_; }

    //! Node name
	string			name_;		
	//! Node identity
	int				id_;
	//! View identity that contains this node 
	int				viewId_;	
	//! Node type
	int				nodeType_;
	//! Precedence when stored in a view
	int				priority_;
	//! Pointer to its parent 
	TeViewNode* 	myParent_;  
	//! Node parent id
	int 			myParentId_;
};


//!  A class to deal with nodes of a view 
/*!  
	 In Terralib, a view is composed of nodes. This is a base class 
	 used to specialize view node types. 

	\sa
     TeView TeViewNodeParams
*/
class TL_DLL TeViewNode
{
public:
	//! Empty constructor
    TeViewNode(const string& name="", TeViewNode* parent=0, const int& view=0, const int& id=0, const int& nodeType=0): 
	  viewNodeParams_(name, id, view, nodeType, 0, parent)
	{ }
	
	//! Constructor
    TeViewNode(const TeViewNodeParams& viewNodeParams): 
	  viewNodeParams_(viewNodeParams)
	{ }

	//! Destructor
	virtual ~TeViewNode()
	{}

	//! Inserts a new child in the tree hierachy. Fails if the object is a leaf
	virtual void add (TeViewNode*) {}

    //!	Remove an existing child identified by its id from the tree hierarchy
    /*!	
		Fails if the object is a leaf
	    Should be used in conjunction with a delete 
	*/	
	virtual TeViewNode* removeID (int /* id */) { return 0; }
 
   //!	Remove an existing child identified by its name from the tree hierarchy
    /*!	
		Fails if the object is a leaf
	    Should be used in conjunction with a delete 
	*/	
	virtual TeViewNode* remove (const string& /*name*/) { return 0; }

	//! Retrieve a node identified by its identifier from the tree structure
	virtual TeViewNode* retrieve (int)  { return 0; }

	//! Returns a pointer to a parent node
	virtual TeViewNode* parent()
	{ return viewNodeParams_.myParent_; }

	//! Returns the identifier of a node parent
	virtual int parentId()
	{
		if (viewNodeParams_.myParent_)
			return viewNodeParams_.myParent_->id(); 
		else
			return viewNodeParams_.myParentId_;
	}

	//! Sets the identification of the parent node
	virtual void parentId(int i) 
	{
		if (viewNodeParams_.myParent_)
			viewNodeParams_.myParent_->id(i);
		viewNodeParams_.myParentId_ = i;
	}

	//! Sets the parent of node
	virtual void setParent ( TeViewNode* node )
	{ 
		viewNodeParams_.myParent_ = node; 
		viewNodeParams_.myParentId_ = (viewNodeParams_.myParent_)? viewNodeParams_.myParent_->id():0;
	}

	//! Returns the identification of a node
	virtual int		id () { return viewNodeParams_.id_; }

	//! Sets the identification of a node
	virtual void	id (int i){ viewNodeParams_.id_ = i; }

	//! Returns the name of a node
	virtual string	name () { return viewNodeParams_.name_; }
	//! Sets the name of a node
	virtual void	name (const string& s) { viewNodeParams_.name_ = s; }

	//! Returns the priority of a node
	virtual int		priority() { return viewNodeParams_.priority_; }
	//! Sets the priority of a node
	virtual void	priority(int i) { viewNodeParams_.priority_ = i; }

	//! Sets the view identification of a node
	virtual void	view (int viewId) { viewNodeParams_.viewId_ = viewId; }
	//! Returns the view identification of a node
	virtual int		view () { return viewNodeParams_.viewId_ ; }

	//! Returns the node type
	virtual int type() { return viewNodeParams_.nodeType_; };
	//! Sets the node type
	virtual void type(const int& t) { viewNodeParams_.nodeType_ = t; }

	//! Moves a node up in the tree structure
	virtual void moveUp ()
	{
		if (viewNodeParams_.myParent_)
			viewNodeParams_.myParent_->moveUp (this);
	}
	
	//! Moves a node down in the tree structure
	virtual void moveDown ()
	{
		if (viewNodeParams_.myParent_)
			viewNodeParams_.myParent_->moveDown (this);
	}

	//! Swaps nodes 
	virtual void swap ( unsigned int, unsigned int ) {}

	//! Sets the nodes visibility 
	virtual void visibility ( int ){}; 

	//! Returns the nodes visibility 
	virtual int visibility(){ return 1; };

	//! Draws a node
	virtual void draw() {}

	//! Sorts the node
	virtual void sort() {}
	
	static TeViewNode*	DefaultObject()
	{	return 0; }

	virtual int getProductId() const
	{	return viewNodeParams_.nodeType_; }

	virtual TeViewNodeParams& viewNodeParams()
	{	return viewNodeParams_; }

	virtual void viewNodeParams(TeViewNodeParams& p)
	{	viewNodeParams_ = p; }


protected:

	virtual void	moveUp (TeViewNode*) {}
	virtual void	moveDown (TeViewNode*) {}

	//! Node parameters
	TeViewNodeParams	viewNodeParams_;
};


//!  This class implements a virtual factory to create view node types. 
/*!  
	 This class is a base virtual factory used to specialize 
	 other factories that create particular node view types. 

	\sa
     TeAbstractFactory TeViewNode TeViewNodeParams
*/
class TL_DLL TeViewNodeFactory : public TeAbstractFactory<TeViewNode,TeViewNodeParams, int>
{
public:
	//! Constructor based on the view node type
	TeViewNodeFactory(const int& nodeType) : 
	  TeAbstractFactory<TeViewNode,TeViewNodeParams, int>(nodeType)
	{ }
};


//! A class to deal with branchs in a view tree structure  
class TL_DLL TeViewTree: public TeViewNode
{
public:

	//! Constructor
	TeViewTree(const string& name=""): TeViewNode(name, 0, 0, 0, (int)TeTREE)
	{ }

	//! Constructor
    TeViewTree(const TeViewNodeParams& params): 
	  TeViewNode(params)
	{ }


	//! Destructor
	virtual ~TeViewTree ();

	//! Sets the nodes visibility 
	virtual void visibility (int vis);

	//! Returns the nodes visibility 
	virtual int visibility ();

	//! Swap nodes
	virtual void swap (unsigned int i, unsigned int j);

	//! Moves a node up in the tree structure
	virtual void moveUp (TeViewNode* node);

	//! Moves a node down in the tree structure
	virtual void moveDown (TeViewNode* node);

	//! Adds a node to the structure
	virtual void add (TeViewNode* node);

    //!	Removes a node identified by its name
	virtual TeViewNode* remove (const string& name);

    //!	Removes a node through its identifier
	virtual TeViewNode* removeID (int id) ;

    //!	Retrieves a node through its index
	virtual TeViewNode* retrieve (unsigned int i) 
	{ return nodes_[i]; }

	//! Draw a node
	virtual void draw ();

	//! Returns the size of the structure
	unsigned int size()
    { return nodes_.size(); }

	//! Returns the vector of View nodes
	vector<TeViewNode*>& nodes()
	{ return nodes_; }

	//! Sort the vector of nodes according to their priorities
	void sort();

	//! Unlinks the nodes from the view tree
	virtual void clear();
	
	/*! Move the given node to the top of it's tree.
	  \param node to be moved
	  \return the number of positions the node has been moved
	*/
	virtual int moveTop (TeViewNode* node);

	/*!	Move the given node to the bottom of it's tree.
	  \param node to be moved
	  \return the number of positions the node has been moved
	*/
	int moveBottom(TeViewNode* node);

	/*!
	Inserts the given node to at the begin of the view tree.
	\param node node to be added
	*/
	virtual void insertFront(TeViewNode* node);

	/*! Look at a View Tree for a node with the given id.
		\param id Id of the node to be found
	*/
	virtual TeViewNode* find (int id);

	/*! Look at a View Tree for a node with the given name.
		\param name name of the node to be found
		\param caseS(optional) true if the given name is case sensitive
	*/
	virtual TeViewNode* find( std::string name, bool caseS = false );

	/*! Returns the boundary box of the tree view themes
	\param projection The returned box coordinates will be in the given projection 
	*/
	virtual TeBox box(bool onlyVisible, TeProjection* projection );

	/*!	Asserts the nodes priorities by the positions.
		Returns true if any priority was updated, otherwise returns false.
	*/
	virtual bool assertsPriorities();

private:
	vector<TeViewNode*> nodes_;
};


//!  This class implements a factory to create view tree objects. 
/*!  
	 This class is a factory that create view nodes 
	 of the type TeTREE, that is, view tree objects.

	\sa
     TeViewNodeFactory TeViewNodeParams TeViewTree  
*/
class TL_DLL TeViewTreeFactory : public TeViewNodeFactory
{
public:
	//! Constructor 
	TeViewTreeFactory() : TeViewNodeFactory((int)TeTREE)
	{}

	//! Created view tree objects 
	TeViewTree* build(TeViewNodeParams* params)
	{	
		TeViewNodeParams auxParams = *params;
		return new TeViewTree(auxParams);	
	}
};

#endif

