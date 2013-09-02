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
#include "TeViewNode.h"
#include "TeTheme.h"
#include "TeLayer.h"
#include "TeProjection.h"

TeBox 
TeViewTree::box(bool onlyVisible, TeProjection* destProjection)
{
	TeBox retval;
	if ( destProjection  == 0 )
		return retval;

	for (vector<TeViewNode*>::iterator child = nodes_.begin(); child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		if( pt->type() == TeTREE )
			updateBox( retval, ( (TeViewTree*)pt)->box( onlyVisible, destProjection ) );
		else
		{
			
			TeAbstractTheme* theme = (TeAbstractTheme*)(pt);

			if( onlyVisible && !( theme->visibility() & 0x00000001 ) )
				continue;

			TeBox themeBox = theme->getThemeBox(); 
			if(!themeBox.isValid()) 
				continue;			
			theme->getThemeProjection()->setDestinationProjection(destProjection);
			TeCoord2D ll = themeBox.lowerLeft();
			TeCoord2D ur = themeBox.upperRight();
			TeBox convertedBox(theme->getThemeProjection()->PC2LL(ll),
							theme->getThemeProjection()->PC2LL(ur));
			ll = convertedBox.lowerLeft();
			ur = convertedBox.upperRight();
			convertedBox = TeBox(destProjection->LL2PC(ll),destProjection->LL2PC(ur));
			updateBox(retval, convertedBox);
		}
	}
	return retval;
}

bool 
TeViewTree::assertsPriorities()
{
	bool updated = false;
	for ( unsigned int i=0; i<nodes_.size(); ++i)
	{
		TeViewNode* node = nodes_[i];
		if ( (node != NULL) && ( node->priority() != (int)i ) )
		{
			node->priority(i);
			updated = true;
		}
		if ( node->type() == TeTREE )
		{
			TeViewTree* tree = (TeViewTree*)node;
			if ( ( tree != NULL ) 
				&& ( tree->assertsPriorities()== true) 
				&& ( updated == false ) )
				updated = true;
		}	
	}
	return updated;
}

TeViewTree::~TeViewTree ()
{
	// For all my children, delete them
    vector<TeViewNode*>::iterator child = nodes_.begin();
    while( child != nodes_.end() ) 
	{
		// Create a temporary pointer to store the child's
		// address
		TeViewNode* pt = (*child);

        // Remove the child from the list
		nodes_.erase(child);
		child = nodes_.begin();

		// Ok, now the child's parent points to NULL and we can
		// safely call the child's destructor
		delete pt;
		pt = 0;
	}
}

void 
TeViewTree::visibility(int vis)
{
	for (vector<TeViewNode*>::iterator child = nodes_.begin();
			child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		pt->visibility( vis );
	}
}

int TeViewTree::visibility()
{
	for (vector<TeViewNode*>::iterator child = nodes_.begin();
			child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		if ( pt->visibility() == 1 )
			return 1;
	}
	return 0;
}

void 
TeViewTree::swap(unsigned int i, unsigned int j)
{
	if (i==j || i>=nodes_.size() || j>=nodes_.size())
		return;
	TeViewNode* temp = nodes_[i];
	nodes_[i] = nodes_[j];
	nodes_[j] = temp;
	nodes_[i]->priority (i);
	nodes_[j]->priority (j);
}


void
TeViewTree::moveUp(TeViewNode* node)
{
	for (unsigned int i=0 ; i<nodes_.size() ; i++)
	{
		if (node == nodes_[i])
		{
			swap (i, i-1);
			return;
		}
	}
}

void 
TeViewTree::moveDown(TeViewNode* node)
{
	if (!node)
		return;

	for (unsigned int i=0 ; i<nodes_.size() ; ++i)
	{
		if (node == nodes_[i])
		{
			swap (i, i+1);
			return;
		}
	}
}

void TeViewTree::add(TeViewNode* node)
{
	node->setParent ( this ); // I am the father
	node->priority(static_cast<int>(nodes_.size()));
	nodes_.push_back (node);
}

//!	Removes a node identified by its name
TeViewNode* 
TeViewTree::remove (const string& name)
{
	vector<TeViewNode*>::iterator child = nodes_.begin();
	while ( child != nodes_.end() )
	{
		TeViewNode* pt = (*child);
		if (pt->name() == name)
		{
			nodes_.erase(child);
			return pt;
		}
		if(pt->type() == TeTREE)
		{
			TeViewNode* result = pt->remove(name);
			if(result) return result;
		}
		++child;
	}
	return 0;
}

//!	Removes a node through its identifier
TeViewNode* 
TeViewTree::removeID (int id) 
{ 
	for (vector<TeViewNode*>::iterator child = nodes_.begin();
			child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		if(pt->id() == id)
		{
			nodes_.erase(child);
			return pt;
		}
		if(pt->type() == TeTREE)
		{
			TeViewNode* result = pt->removeID(id);
			if(result) return result;
		}
	}
	return 0;
}

void 
TeViewTree::draw ()
{				  
	vector<TeViewNode*>::iterator child = nodes_.begin();
	while ( child != nodes_.end() )
	{
		TeViewNode* pt = (*child);
		pt->draw();
		++child;
	}
}

void 
TeViewTree::sort()
{
	int i, j;
	for (i = 0; i < (int)size() - 1; ++i)
	{
		for (j = i+1; j < (int)size(); ++j)
		{
			if (nodes_[i]->priority() > nodes_[j]->priority())
			{
				TeViewNode* temp = nodes_[i];
				nodes_[i] = nodes_[j];
				nodes_[j] = temp;
			}
		}
	}
}

void 
TeViewTree::clear()
{
	// For all my children, delete them
    vector<TeViewNode*>::iterator child = nodes_.begin();
    while( child != nodes_.end() ) 
	{
		// Create a temporary pointer to store the child's
		// address
		TeViewNode* pt = (*child);

        // Remove the child from the list
		nodes_.erase(child);

		if( pt->type() == TeTREE )
			((TeViewTree*)pt)->clear();
		
		child = nodes_.begin();
	}
}

int 
TeViewTree::moveTop (TeViewNode* node)
{
	TeViewNode* parent = node->parent();
	if(parent == NULL) 
		return -1;
	if(parent->type() != TeTREE) 
		return -1;
	TeViewTree* tree = (TeViewTree*)parent;
	int i = 0;
	while(tree->retrieve(0) != node && tree->retrieve(0) != NULL)
	{
		tree->moveUp(node);
		++i;
	}
	return i;
}

int 
TeViewTree::moveBottom(TeViewNode* node)
{
	TeViewNode* parent = node->parent();
	if (parent == 0 || parent->type() != TeTREE)
		return -1;
	TeViewTree* tree = (TeViewTree*)parent;
	int i = 0;
	while(tree->nodes().back() != node && tree->nodes().back() != 0)
	{
		tree->moveDown(node);
		++i;
	}
	return i;
}


void 
TeViewTree::insertFront(TeViewNode* node)
{
	node->setParent( this ); // I am the father
	nodes_.insert(nodes_.begin(), node);
	int ind = 0;
	for (vector<TeViewNode*>::iterator child = nodes_.begin();
			child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		if ( pt )
			pt->priority( ind++ ); 
	}
}

TeViewNode* 
TeViewTree::find (int id) 
{ 
	for (vector<TeViewNode*>::iterator child = nodes_.begin();
			child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		if(pt->id() == id)
			return pt;
		if(pt->type() == TeTREE)
		{
			TeViewNode* result = ((TeViewTree*)(pt))->find(id);
			if(result) 
				return result;
		}
	}
	return 0;
}
TeViewNode* 
TeViewTree::find( std::string name, bool caseS) 
{ 
	for (vector<TeViewNode*>::iterator child = nodes_.begin();
			child != nodes_.end(); ++child)
	{
		TeViewNode* pt = (*child);
		if( TeStringCompare( pt->name(), name, caseS ) )
			return pt;
		if( pt->type() == TeTREE )
		{
			TeViewNode* result = ((TeViewTree*)(pt))->find( name, caseS );
			if (result) 
				return result;
		}
	}
	return 0;
}
