/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DisplayManager.cc
    \brief Implementation of the Template class DisplayManager.
    
    Magics Team - ECMWF 2007
    
    Started: Fri 9-Mar-2007
    
    Changes:
    
*/



#include "DisplayManager.h"
#include "BasicSceneObject.h"


using namespace magics;

DisplayManager::DisplayManager()  :   fortran_(false), x_(-1), y_(-1) 
{
}


DisplayManager::~DisplayManager() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void DisplayManager::print(ostream& out)  const
{
	out << "DisplayManager[";
	out << "]";
}


void DisplayManager::operator()(BasicSceneObject& object,  BasicGraphicsObjectContainer& list)
{
	switch (object.display()) {
		case INLINE: 
				fortran_ ? (this->*style_)(object, list) : addInline(object, list);
				break;
		case BLOCK:
				fortran_ ? (this->*style_)(object, list) : addBlock(object, list);
				break;
		default:
				break;		
	}
}



void DisplayManager::topHorizontal(BasicSceneObject& object, BasicGraphicsObjectContainer& list)
{
	object.resize();
	if ( x_ == -1 && y_ == -1 ) {
		// Calculate the position
		x_ = 0;
		y_ = 100 - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ = object.width();
		object.firstPage(list);
		return;
	}	
	// Try to fit to the right! 
	if ( x_ + object.width() <= 100 ) {
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	
	// Go to the line! 
	if ( y_ - object.height() >= 0 ) {
		x_ = 0;
		y_ = y_ - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	// Ask for a new page! 
	x_ = 0;
	y_ = 100 - object.parent().topOffset() - object.height();
	object.x(x_);
	object.y(y_);
	object.askNewPage(list);
	x_ = object.width();
}


void DisplayManager::addBlock(BasicSceneObject& object, BasicGraphicsObjectContainer& list)
{
	object.resize();
	if ( x_ == -1 && y_ == -1 ) {
		// Calculate the position
		x_ = 0;
		y_ = 100 - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ = object.width();
		object.firstPage(list);
		return;
	}	
	
    // go to the next list! 
	if ( y_ - object.height() > 0 ) {
		x_ = 0;
		y_ = y_ - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	// ask for a new page	
	x_ = 0;
	y_ = 100 - object.parent().topOffset() - object.height();
	object.x(x_);
	object.y(y_);
	object.askNewPage(list);
	x_ = object.width();
}


void DisplayManager::addInline(BasicSceneObject& object, BasicGraphicsObjectContainer& list)
{
	object.resize();
	if ( x_ == -1 && y_ == -1 ) {
		// Calculate the position
		x_ = 0;
		y_ = 100 - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ = object.width();
		object.firstPage(list);
		return;
	}	
	// Try to fit to the right! 
	if ( x_ + object.width() <= 100 ) {
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	
	// Go to the line! 
	if ( y_ - object.height() >= 0 ) {
		x_ = 0;
		y_ = y_ - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	// Ask for a new page! 
	x_ = 0;
	y_ = 100 - object.parent().topOffset() - object.height();
	object.x(x_);
	object.y(y_);
	object.askNewPage(list);
	x_ = object.width();
}

void DisplayManager::bottomVertical(BasicSceneObject& object, BasicGraphicsObjectContainer& list)
{
	object.resize();
	if ( x_ == -1 && y_ == -1 ) {
		// Calculate the position
		x_ = object.x();
		y_ = object.y();
		object.x(x_);
		object.y(y_);
		y_ += object.height();
		return;
	}	
	// Try to fit above! 
	if ( y_ + object.height() <= 100 ) {
		object.x(x_);
		object.y(y_);
		y_ += object.height();
		object.currentPage(list);
		return;
	}
	
	// Go to the next column! 
	if ( x_ + object.width() < 100 ) {
		x_ += object.width();
		y_ = object.y();		
		object.x(x_);
		object.y(y_);
		y_ += object.height();
		object.currentPage(list);
		return;
	}
	// Ask for a new page! 
	x_ = object.x();
	y_ = object.y();
	object.x(x_);
	object.y(y_);
	object.askNewPage(list);
	y_ += object.height();
}

void DisplayManager::bottomHorizontal(BasicSceneObject&, BasicGraphicsObjectContainer&)
{
}

void DisplayManager::nothing(BasicSceneObject& object, BasicGraphicsObjectContainer& list)
{
	if ( object.needNewPage() ) 
		object.askNewPage(list);
}

void DisplayManager::topVertical(BasicSceneObject& object, BasicGraphicsObjectContainer& list)
{
	object.resize();
	if ( x_ == -1 && y_ == -1 ) {
		// Calculate the position
		x_ = 0;
		y_ = 100 - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ = object.width();
		object.firstPage(list);
		return;
	}	
	// Try to fit to the right! 
	if ( x_ + object.width() <= 100 ) {
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	
	// Go to the line! 
	if ( y_ - object.height() >= 0 ) {
		x_ = 0;
		y_ = y_ - object.parent().topOffset() - object.height();
		object.x(x_);
		object.y(y_);
		x_ += object.width();
		object.currentPage(list);
		return;
	}
	// Ask for a new page! 
	x_ = 0;
	y_ = 100 - object.parent().topOffset() - object.height();
	object.x(x_);
	object.y(y_);
	object.askNewPage(list);
	x_ = object.width();
}


	
void DisplayManager::style(const string& style, const string& start, const string& direction)
{
	fortran_ = true;
	if ( magCompare(style, "positional") ) {
		style_ = &DisplayManager::nothing;
		return;
	}
	if ( magCompare(start, "top") )
		if ( magCompare(direction, "vertical") )
			style_ = &DisplayManager::topVertical;
		else 
			style_ =  &DisplayManager::topHorizontal;
	else
		if ( magCompare(direction, "vertical") )
			style_ = &DisplayManager::bottomVertical;
		else 
			style_ = &DisplayManager::bottomHorizontal;
}
