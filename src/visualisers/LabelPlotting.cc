/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LabelPlotting.cc
    \brief Implementation of the Template class LabelPlotting.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 2-Feb-2004
    
    Changes:
    
*/

#include "LabelPlotting.h"
#include "Text.h"
#include "GridPlotting.h" 
#include "BasicSceneObject.h" 
using namespace magics;

LabelPlotting::LabelPlotting() :layer_(0)
{
}

LabelPlotting::~LabelPlotting() 
{
	if (layer_) layer_->clear();
}

void LabelPlotting::prepare(NoGridPlotting& grid)
{

	
	
	if ( *colour_ == "UNDEFINED" ) {

		colour_ = unique_ptr<Colour>(new Colour(grid.colour()));
	}
	
	if ( longitudes_.empty() )
		grid.longitudes(longitudes_, lonFrequency_);

	if ( latitudes_.empty() )
		grid.latitudes(latitudes_, latFrequency_);

    
    
        
}


/*!
 Class information are given to the output-stream.
*/		
void LabelPlotting::print(ostream& out)  const
{
	out << "LabelPlotting[";
	LabelPlottingAttributes::print(out);
	out << "] ";
}

void NoLabelPlotting::label(Transformation& transformation)
{
	transformation.needTopAxis(false);
}
void LabelPlotting::label(Transformation& transformation)
{
	transformation.needTopAxis(true);
}
