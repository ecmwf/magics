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

/*! \file SceneNode.cc
    \brief Implementation of the Template class SceneNode.
    
    Magics Team - ECMWF 2007
    
    Started: Thu 1-Mar-2007
    
    Changes:
    
*/



#include "SceneNode.h"
#include "LayoutManager.h"
#include "Dimension.h"
#include "TextVisitor.h"
#include "LegendVisitor.h"
#include "Layer.h"

using namespace magics;

SceneNode::SceneNode() : legend_(0)
{
	static int i = 0;

    
    ostringstream n;
	n << "Page" << i;
	name_ = n.str();
	i++;
    layout_ = new SceneLayout();
    layout_->name(name_);
    layout_->id(iconId_); // For Metview
}


SceneNode::~SceneNode() 
{
	
}


/*!
 Class information are given to the output-stream.
*/		
void SceneNode::print(ostream& out)  const
{
	out << "SceneNode[" << name_;
	
		out << "->" << parent_->name();
	
	out << "]";
}
void SceneNode::getReady()
{
}
void SceneNode::text(TextVisitor* text)
{
	MagLog::dev() << " SceneNode::text -->" << endl;
	((BasicPositionalObject*)text)->parent(this);
	text->getReady(); //to calcuilate the dimension
		
	 (*manager_)(this, (BasicPositionalObject*)text);
	texts_.push_back(text);
}

void SceneNode::legend(LegendVisitor* legend)
{
	legend_ = legend; 
	((BasicPositionalObject*)legend)->parent(this);
	legend->getReady(); //to calcuilate the dimension
		
	(*manager_)(this, (BasicPositionalObject*)legend);
	
}

void SceneNode::visit(BasicGraphicsObjectContainer& tree)
{
	tree.push_back(layout_);
	layout_->blankIt(); 
    SceneLayer* layer = new SceneLayer();
    for ( vector<TextVisitor*>::iterator text = texts_.begin(); text != texts_.end(); ++text)
    	layer->text(*text);
    layer->legend(legend_);
    layer->setMagicsMode(mode());
    layout_->push_back(layer);
    layout_->id(iconId_); // For Metview
    
  
	dispatch(*layer); 
	
	layout_->frameIt();  
	
}

FortranSceneNode::FortranSceneNode() : sceneLayer_(0)
{

}

void FortranSceneNode::visit(BasicGraphicsObjectContainer& tree)
{
	if ( !sceneLayer_ ) {
		tree.push_back(layout_);
		layout_->blankIt();
		sceneLayer_ = new SceneLayer();
		sceneLayer_->state(new_layer);
		sceneLayer_->setMagicsMode(mode());
		layout_->push_back(sceneLayer_);
		page_id_->visit(*layout_);
		layout_->id(iconId_); // For Metview
	}
#ifdef MAG_NEXT
	else 
    {
		sceneLayer_->state(geometry_changed);
		//layout_->clear();
		//layout_->push_back(sceneLayer_);
	}
#endif

	dispatch(*sceneLayer_);
	
	layout_->frameIt();  
	
}


void FortranSceneNode::getReady()
{	
	assert(parent_);
	
	// Make sure that the dimensions are not bigger that the paper size!

	if ( width_ > parent_->absoluteWidth() )
		width_ = parent_->absoluteWidth();
	if ( height_ > parent_->absoluteHeight() )
			height_ = parent_->absoluteHeight();


	double x =  adjustDimension(x_, 0, parent_->absoluteWidth());
	double y = adjustDimension(y_, 0, parent_->absoluteHeight());
    double width = adjustDimension(width_, 100, parent_->absoluteWidth());
	double height = adjustDimension(height_, 100, parent_->absoluteHeight());
	
	layout_->x(x);
	layout_->y(y);
	layout_->width(width);
	layout_->height(height);
	
    
	layout_->frame(false, FortranSceneNodeAttributes::frame_,*frame_colour_, frame_line_style_, frame_thickness_);
  
	BasicSceneObject::getReady();
}

FortranSceneNode::~FortranSceneNode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void FortranSceneNode::print(ostream& out)  const
{
	out << "FortranSceneNode[";
	BasicSceneObject::print(out);
	FortranSceneNodeAttributes::print(out);
	out << "]";
}

XmlSceneNode::XmlSceneNode() 
{
}


XmlSceneNode::~XmlSceneNode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void XmlSceneNode::print(ostream& out)  const
{
	out << "XmlSceneNode[";
	out << "]";
}



void XmlSceneNode::getReady()
{
	
	MagLog::dev() << " SceneNode::getReady() \n";
	MagLog::dev() << "XmlSceneNode::getReady() \n";
	assert (parent_);
	Dimension bottom(bottom_, parent_->absoluteWidth(), 0);
	Dimension left(left_, parent_->absoluteHeight(), 0);
	Dimension width(width_, parent_->absoluteWidth(), 100);
	Dimension height(height_, parent_->absoluteHeight(), 100);
	
	Dimension mb(margin_bottom_, width.absolute(), 0);
	Dimension ml(margin_left_, height.absolute(), 0);
	Dimension mr(margin_right_, width.absolute(), 0);
	Dimension mt(margin_top_, height.absolute(), 0);
	

	
	layout_->x(left.percent()); // Add alos the margin!
	layout_->y(bottom.percent()); // Add alos the margin!
	layout_->width(width.percent());
	layout_->height(height.percent());
	
	layout_->display(display_);
	
	

	
	layout_->frame(false, border_, *border_colour_, border_style_, border_thickness_);
	
	

	BasicSceneObject::getReady();
	
}



