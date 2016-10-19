/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PolyCoast.h
    \brief Definition of the Template class CoastPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 2-Feb-2004
    
    Changes:
    
*/
#ifndef PolyCoast_H
#define PolyCoast_H

#include "magics.h"

#include "Polyline.h"

namespace magics
{
	

class PolyCoast : public Polyline
{
public :
	PolyCoast(): level_(0), greenwich_(0), area_(0)
		{ this->winding_ = -1; }
	~PolyCoast() {}

	virtual PolyCoast* clone() const
	{
		PolyCoast* poly = new PolyCoast();

		poly->coastlines_ = coastlines_;
		poly->setColour(this->getColour());
		poly->setThickness(this->thickness_);
		poly->setDashLength(this->dash_length_);
		poly->setLineStyle(this->style_);
		
		poly->level(this->level_);
		poly->setFillColour(this->getFillColour());
		poly->setFilled(this->fill_);   
		if (this->shading_) poly->setShading(this->shading_->clone());
		poly->insert(poly->begin(), this->begin(), this->end());
		poly->setMinX(this->getMinX());  // copy the bounding box
		poly->setMinY(this->getMinY());  // copy the bounding box
		poly->setMaxX(this->getMaxX());  // copy the bounding box
		poly->setMaxY(this->getMaxY());  // copy the bounding box
		return poly;
	}
	
	virtual Polyline* getNew() const
	{
		PolyCoast* poly = new PolyCoast();
		
		poly->setColour(this->getColour());
		poly->setThickness(this->thickness_);
		poly->setDashLength(this->dash_length_);
		poly->setLineStyle(this->style_);
		poly->level(this->level_);
		poly->setFillColour(this->getFillColour());
		poly->setFilled(this->fill_);   
		if (this->shading_) poly->setShading(this->shading_->clone()); 
	
		return poly;
	}
	
	virtual Polyline* getShade() const
		{
		if ( !this->fill_ ) return 0;
		
			PolyCoast* poly = new PolyCoast();
			
			poly->setColour(Colour("none"));
			poly->setThickness(0);
			poly->setDashLength(this->dash_length_);
			poly->setLineStyle(this->style_);
			poly->level(this->level_);
			poly->setFillColour(this->getFillColour());
			poly->setFilled(this->fill_);   
			if (this->shading_) poly->setShading(this->shading_->clone()); 
		
			return poly;
		}
	virtual Polyline* getContour() const
		{
		
		
			PolyCoast* poly = new PolyCoast();
			
			poly->setColour(this->getColour());
			poly->setThickness(this->thickness_);
			poly->setDashLength(this->dash_length_);
			poly->setLineStyle(this->style_);
			poly->level(this->level_);
			poly->setFillColour(this->getFillColour());
			poly->setFilled(false);   
			poly->setShading(0); 
		
			return poly;
		}

	int level() const { return level_; }
	void level(int level) { level_ = level; }
	int greenwich() const { return greenwich_; }
	void greenwich(int greenwich) { greenwich_ = greenwich; }
	
	vector<UserPoint>& coastlines() { return coastlines_; }
	
protected:
	int level_;
	int greenwich_;
	double area_;
	vector<UserPoint> coastlines_;

	
};
} // end namespace
#endif
