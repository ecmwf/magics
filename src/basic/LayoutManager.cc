/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LayoutManager.h
    \brief Implementation of the Template class LayoutManager.
    
    Magics Team - ECMWF 2009
    
    Started: Mon 19-Jan-2009
    
    Changes:
    
*/



#include "LayoutManager.h"
#include "BasicSceneObject.h"
#include "Layout.h"

using namespace magics;


LayoutManager::LayoutManager() : newpage_(false)
{
}


LayoutManager::~LayoutManager() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void LayoutManager::print(ostream& out)  const
{
	out << "LayoutManager[";
	out << "]";
}

BottomVerticalLayoutManager::BottomVerticalLayoutManager()
{
	x_ = 0;
	y_ = 0;
}

BottomVerticalLayoutManager::~BottomVerticalLayoutManager()
{
}
 
BasicSceneNode* MagMLLayoutManager::block(BasicSceneNode* parent, BasicPositionalObject* node)
{
	Layout& layout = node->layout();
		// first fits in width! 
		double width = layout.x() + layout.width();
		double height = layout.y() + layout.height();
		if (100 - y_ - height >=  0 ) { 
		   if ( x_ + width  <=100 ) {
				// it fits perfectly! we set the postion for the node
				layout.x(x_ +  layout.x() );				
				layout.y(100 - y_ - layout.height());
				//we prepare for another row!
				x_ = 100;
				maxy_ = std::max( maxy_,   y_ + height);
				
				
				return parent;
		   }
		   else { // create another row!
			   y_ = maxy_;
			   x_ = 0;
			   return inline_display(parent, node);	
		   }
		}
		// Do not fit on this Layout!
		return parent->newNode(node);
}

MagMLLayoutManager::MagMLLayoutManager()
{
    x_    = 0;
    y_    = 0;
    maxy_ = 0; 
    maxx_ = 0;
}

BasicSceneNode* MagMLLayoutManager::inline_display(BasicSceneNode* parent, BasicPositionalObject* node)
{
	Layout& layout = node->layout();
	// first fits in width! 
	const double x = ( layout.x() ? layout.x() : x_);
	const double y = ( layout.y() ? layout.y() : y_);
	const double width  = x + layout.width();
	const double height =     layout.height();

	if (100 - y_ - height >=  0 )
	{ 
	   if ( width  <=100 )
	   {
			x_ = layout.x() ? layout.x() : x_;
			y_ = layout.x() ? layout.y() : y_;
			// it fits perfectly! we set the postion for the node
			layout.x(x_);				
			layout.y(100 - y_ - layout.height());
			x_ = width;
			maxy_ = std::max( maxy_,   y_ + height);
			return parent;
	   }
	   else { // create another row!
		   y_ = maxy_;
		   x_ = 0;
		   return inline_display(parent, node);	
	   }
	}
	// Do not fit on this Layout!
	return parent->newNode(node);
}

BasicSceneNode* MagMLLayoutManager::absolute(BasicSceneNode* parent, BasicPositionalObject*)
{
	return parent;
}

std::map<DisplayType, MagMLLayoutManager::Action> MagMLLayoutManager::actions_;

BasicSceneNode* MagMLLayoutManager::operator()(BasicSceneNode* parent, BasicPositionalObject* node)
{
	 Layout& layout = node->layout();
	 
	 DisplayType display = layout.display();
	 
	 if ( actions_.empty() ) {
			actions_[M_DT_INLINE] = &MagMLLayoutManager::inline_display;
			actions_[M_DT_ABSOLUTE] = &MagMLLayoutManager::absolute;
			actions_[M_DT_BLOCK] = &MagMLLayoutManager::block;
	 }
	 
	 std::map<DisplayType, Action>::iterator action = actions_.find(display);

	 if ( action == actions_.end() )
	 {
		 return inline_display(parent, node);
	 }
	 return (this->*action->second)(parent, node);
}
 
 BasicSceneNode*  BottomVerticalLayoutManager::operator()(BasicSceneNode* parent, BasicPositionalObject* node)
{
	Layout& layout = node->layout();
	// first fits in width! 
	if ( x_ + layout.width()  <=100 ) {
	   if (y_ +layout.height() <=100 ) { 
			// it fits perfectly! we set the postion for the node
			layout.x(x_);
			layout.y(y_);
			y_ += layout.height() + gapy_;
			return parent;
	   }
	   else { // create another column
		   x_ += layout.width() + gapx_;
		   y_ = 0;
		   // try again! 
		   return (*this)(parent, node);	
	   }
	}
	// Nothing to do on this page!

	return parent->newNode(node);
}
 
BottomHorizontalLayoutManager::BottomHorizontalLayoutManager()
{
 	x_ = 0;
 	y_ = 0;
}

BottomHorizontalLayoutManager::~BottomHorizontalLayoutManager()
{
}
  
BasicSceneNode*  BottomHorizontalLayoutManager::operator()(BasicSceneNode* parent, BasicPositionalObject* node)
{
	  Layout& layout = node->layout();
	// first fits in width! 

	   if (y_ + layout.height() <=  100 ) { 
		   if ( x_ + layout.width()  <=100 ) {
			// it fits perfectly! we set the postion for the node
			layout.x(x_);				
			x_=  x_ + layout.width();
			layout.y(y_);
			return parent;
	   }
	   else { // create another row
		   x_ = 0;
		  y_ = y_ + layout.height() + gapy_;
		   return (*this)(parent, node);	
	   }
	}
	// Do not fit on this Layout!
	return parent->newNode(node);
}

TopVerticalLayoutManager::TopVerticalLayoutManager()
{
	x_ = 0;
	y_ = 100;
}

TopVerticalLayoutManager::~TopVerticalLayoutManager()
{
}

BasicSceneNode*  LayoutManager::operator()(BasicSceneNode* parent, BasicPositionalObject* node)
{
	if ( newpage_ ) {
		newpage_ = false; 
		return parent->newNode(node);
	}
	else 
		return parent;
}

TopHorizontalLayoutManager::TopHorizontalLayoutManager()
{
	x_ = 0;
	y_ = 100;
}

TopHorizontalLayoutManager::~TopHorizontalLayoutManager()
{ 
}

BasicSceneNode*  TopHorizontalLayoutManager::operator()(BasicSceneNode* parent, BasicPositionalObject* node)
{
	  Layout& layout = node->layout();
	// first fits in width! 

	   if (y_ - layout.height() >=  0 ) { 
		   if ( x_ + layout.width()  <=100 ) {
			// it fits perfectly! we set the postion for the node
			layout.x(x_);				
			x_=  x_ + layout.width();
			layout.y(y_- layout.height());
			return parent;
	   }
	   else { // create another column
		   x_ = 0;
		  y_ -= layout.height() - gapy_;
		   return (*this)(parent, node);	
	   }
	}
	// Do not fit on this Layout!
	return parent->newNode(node);
}

 
LayoutManager* LayoutManager::manager(const string& type, const string& start, const string& direction)
{
	 if ( magCompare(type, "magml") )
		 return new MagMLLayoutManager();
	 if ( magCompare(type, "automatic") ) {
		 if ( magCompare(start, "bottom") ) {
			 if ( magCompare(direction, "vertical") ) 
				 return new BottomVerticalLayoutManager();
			return new BottomHorizontalLayoutManager();
		 }
		 
		if ( magCompare(direction, "vertical") ) 
				return new TopVerticalLayoutManager(); 
		return new TopHorizontalLayoutManager();			
    }
	 

	 return new LayoutManager();
}
 
BasicSceneNode*  TopVerticalLayoutManager::operator()(BasicSceneNode* parent, BasicPositionalObject* node)
{
 	Layout& layout = node->layout();
 	// first fits in width! 
 	if ( x_ + layout.width()  <=100 ) {
 	   if (y_ - layout.height() >=  0 ) { 
 			// it fits perfectly! we set the postion for the node
 			layout.x(x_);				
 			y_ -= layout.height() - gapy_;
 			layout.y(y_);
 			return parent;
 	   }
 	   else { // create another column
 		   x_ += layout.width() + gapx_;
 		   y_ = 100;
 		   return (*this)(parent, node);	
 	   }
 	}
 	// Do not fit on this Layout!
 	return parent->newNode(node);
}
