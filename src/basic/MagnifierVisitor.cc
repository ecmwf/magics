/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagnifierVisitor.h
    \brief Implementation of the Template class MagnifierVisitor.
    
    Magics Team - ECMWF 2009
    
    Started: Tue 27-Jan-2009
    
    Changes:
    
*/



#include "MagnifierVisitor.h"
#include "BaseDriver.h"
#include "Transformation.h"
#include "AnimationRules.h"
#include "MagicsFormat.h"

using namespace magics;

NoMagnifierVisitor::NoMagnifierVisitor() : owner_(0)
{
}


NoMagnifierVisitor::~NoMagnifierVisitor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void NoMagnifierVisitor::print(ostream& out)  const
{
	out << "NoMagnifierVisitor[";
	out << "]";
}

MagnifierVisitor::MagnifierVisitor() : values_(0), more_(0)
{
	static int count = 0;
	
		ostringstream n;
		n << "Magnifier" << count;
		name(n.str());
		count++;
		
}


MagnifierVisitor::~MagnifierVisitor() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void MagnifierVisitor::print(ostream& out)  const
{
	out << "MagnifierVisitor[";
	Layout::print(out);
	out << "]";
}

void MagnifierVisitor::visit(BasicGraphicsObjectContainer& tree)
{

	tree.push_back(this);
	// the Layout has been added to a Container, it will be delted automatically!
	
}

void MagnifierVisitor::add(const PaperPoint& point)
{
	if ( !values_ ) {
		values_ = new TextSymbol();
		MagFont font(text_font_name_);
		font.colour(*text_font_colour_);
		font.size(text_font_size_);
		values_->font(font);
		values_->position(TextSymbol::M_BELOW);
		values_->setSymbol(symbol_name_); // A little cross
		values_->setHeight(symbol_height_); 
		values_->setColour(*symbol_colour_); 				
		this->push_back(values_);
	}
	ostringstream nice;
	nice << MagicsFormat(text_format_, point.value());   
	       
	values_->push_back(point, nice.str());
}
void MagnifierVisitor::addMore(const PaperPoint& point)
{
	if ( !more_ ) {
		more_ = new Symbol();
		more_->setSymbol(hidden_symbol_name_); // A little dot
		more_->setHeight(hidden_symbol_height_); 
		more_->setColour(*hidden_symbol_colour_); 				
		this->push_back(more_);
	}
	more_->push_back(point);
}

void MagnifierVisitor::redisplay(const BaseDriver& driver) const 
{
	
	
	driver.redisplay(*this); 
}

void MagnifierVisitor::redisplay(const BaseDriver& driver, vector<PaperPoint>& pp,float xres,float yres) const
{
	   
	   values_ = 0;
	   more_ = 0;
		
		// Update the view from the transfomation
		
		// visit the owner to get the info!
		ASSERT(owner_);
		double xmin = pp.front().x();
		double xmax = pp.front().x();
		double ymin = pp.front().y();
	    double ymax = pp.front().y();
	    
	    for (vector<PaperPoint>::iterator p = pp.begin(); p != pp.end(); ++p ) {
	    	xmin = std::min(xmin, p->x());
	    	xmax = std::max(xmax, p->x());	
	    	ymin = std::min(ymin, p->y());
	    	ymax = std::max(ymax, p->y());
	    }
	    
	    	    transformation_->filterView(xmin, xmax, ymin, ymax, xres, yres);
		
	    	    owner_->visit(*const_cast<MagnifierVisitor*>(this));
		if ( more_ ) 
			driver.redisplay(*more_); 
		if ( values_ ) 
			driver.redisplay(*values_); 
		
		
}
	




