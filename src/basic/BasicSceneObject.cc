/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file BasicSceneObject.cc
    \brief Implementation of the Template class BasicSceneObject.
    
    Magics Team - ECMWF 2007
    
    Started: Thu 1-Mar-2007
     
    Changes:
    
*/


#include "BasicSceneObject.h"
#include "BasicSceneVisitor.h"
#include "Layout.h"
#include "LayoutManager.h"
#include "Layer.h"


using namespace magics;

BasicSceneObject::BasicSceneObject(BasicSceneObject* parent) : 
    parent_(parent)
{
	if ( parent_ ) 
		parent_->items_.push_back(this);
}


BasicSceneObject::~BasicSceneObject() 
{
	//delete the visitors...
}

/*!
 Class information are given to the output-stream.
*/		
void BasicSceneObject::print(ostream& out)  const
{
	out << "BasicSceneObject[" << name() << endl;
	string tab = "--->"; 
	BasicSceneObject* parent = parent_;
	while ( parent ) {
		out << tab << parent->name() << endl;
		tab = "--" + tab;
		parent = parent->parent_;
	}
	out << "]" << endl;
}

BasicSceneNode::BasicSceneNode() : layout_(0)
{
	string type, start, direction;
	ParameterManager::get("layout", type); 
	ParameterManager::get("plot_start", start);
	ParameterManager::get("plot_direction", direction);
	manager_ = LayoutManager::manager(type, start, direction);
}

BasicSceneNode::BasicSceneNode(Layout* layout) : layout_(layout)
{
	string type, start, direction;
	ParameterManager::get("layout", type); 
	ParameterManager::get("plot_start", start);
	ParameterManager::get("plot_direction", direction);
	manager_ = LayoutManager::manager(type, start, direction);
}

BasicSceneNode::~BasicSceneNode() {
	//if ( layout_ ) delete layout_;		
	if ( manager_ ) delete manager_;
}

BasicSceneNode* BasicSceneNode::clone()
{
	BasicSceneNode* node = new BasicSceneNode();
	node->layout_ = layout_->clone();
	node->manager_ = manager_->clone();
	return node;
}

void  BasicSceneObject::resolve()
{
	// WE first do a copy of the list...
	vector<BasicSceneObject*> items;
	for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item) 
		items.push_back(*item);
	for ( vector<BasicSceneObject*>::iterator item = items.begin(); item != items.end(); ++item)
		(*item)->resolve();
}

bool  BasicSceneObject::needLegend()
{
	for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item)
			if ( (*item)->needLegend() )
				return true;
	return false;
}

void  BasicSceneObject::getReady(const LegendVisitor& legend)
{
	for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item)
			(*item)->getReady(legend);
}
BasicSceneNode* BasicSceneNode::newNode(BasicPositionalObject* node)
{
	//MagLog::dev() << "BasicSceneNode::clone-->from " << *this << endl;	

	BasicSceneNode* newnode = clone();

	parent_->insert(newnode);
	(*(newnode->manager_))(newnode, node);

	// The new Node had been inserted in the tree! 
	return newnode;
}
	
BasicSceneNode* BasicSceneNode::insert(BasicPositionalObject* node)
{
	node->parent(this);
	node->getReady(); //to calcuilate the dimension
	
	BasicSceneNode* parent = (*manager_)(this, node);
	//parent is the node when to push_back the new! 
	node->orphan();
	parent->push_back(node);
 	return parent;
}


double BasicPositionalObject::adjustDimension(double x, double def, double parent)
{
	//MagLog::dev() << *this << endl;	
	//MagLog::dev() << *parent_ << endl;
	if ( x == -1) {
	    MagLog::dev() << "adjustDimension[" << x << ", " << def << ", " << parent << "]=" << def << "%" << endl;
	    return def;
	}
	MagLog::dev() << "adjustDimension[" << x << ", " << def << ", " << parent << "]=" << (x/parent)*100 << "%" << endl;
	double dim = (x/parent)*100;
	// can not return more thean 100!

	return dim;
//	return dim > 100 ? 100 : dim;
}

double BasicSceneNode::absoluteWidth()    const
{
	return (parent_->absoluteWidth()*layout_->width())/100.;
}

double BasicSceneNode::absoluteHeight()   const
{
	return (parent_->absoluteHeight()*layout_->height())/100.;
}

void BasicSceneNode::newpage()
{
	assert(manager_);
	manager_->newpage();
}

void BasicSceneNode::getReady()
{	
	static int i = 0;
	ostringstream n;
	n << "basic" << i;
	name_ = n.str();
	i++;
	MagLog::dev() << "new getReady-->" << name_ << endl;



	layout_->name(name_);
}

void EmptySceneObject::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors)
{
	// First we create the layer!
	// and push It to the parent layer! 
	StaticLayer* empty = new StaticLayer(this);
	empty->name("EmptySceneObject");
	layer.add(empty);
	// Just set the layout!!!
	for  (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
		empty->set(*visitor);
	}
}

EmptySceneObject::EmptySceneObject()
{
}

EmptySceneObject::~EmptySceneObject()
{
}

void EmptySceneObject::print(ostream&) const
{
}
