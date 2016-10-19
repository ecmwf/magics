/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GraphShadeStyle.cc
    \brief Implementation of the Template class GraphShadeStyle.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 17-Aug-2006
    
    Changes:
    
*/

#include "GraphShadeStyle.h"
#include "Polyline.h"
#include "PaperPoint.h"
#include "PaperPoint.h"


using namespace magics;

GraphShadeStyle::GraphShadeStyle() 
{
}


GraphShadeStyle::~GraphShadeStyle() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void GraphShadeStyle::print(ostream& out)  const
{
	out << "GraphShadeStyle[";
	out << "]";
}

void HatchGraphShadeStyle::print(ostream& out)  const
{
	out << "HatchGraphShadeStyle[";
	GraphShadeStyle::print(out);
	HatchGraphShadeStyleAttributes::print(out);
	out << "]";
}

void DotGraphShadeStyle::print(ostream& out)  const
{
	out << "DotGraphShadeStyle[";
	GraphShadeStyle::print(out);
	DotGraphShadeStyleAttributes::print(out);
	out << "]";
}

void GraphShadeStyle::operator()(Polyline& box)
{
	box.setFillColour(*colour_);
	box.setFilled(true);
	ShadingProperties* shading = new FillShadingProperties();
	
	box.setShading(shading);
}

void DotGraphShadeStyle::operator()(Polyline& box)
{
	box.setFillColour(*colour_);
	box.setFilled(true);
	DotShadingProperties* shading = new DotShadingProperties();
	shading->size_ = size_;
	shading->density_ = density_;
	
	box.setShading(shading);
}

void HatchGraphShadeStyle::operator()(Polyline& box)
{
	box.setFillColour(*colour_);
	box.setFilled(true);
	HatchShadingProperties* shading = new HatchShadingProperties();
	if ( index_ < 1 || index_ > 6) {
		static bool msg = true;
		if ( msg ) {
			msg = false;
			MagLog::warning() << " Hatch shading index should be between 1 and 6: Found [" << index_ << "], revert to default [1] " << endl;
		}
		index_ = 1;
	}
	shading->index_ = index_;
	
	box.setShading(shading);
}

