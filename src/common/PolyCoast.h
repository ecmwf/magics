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
