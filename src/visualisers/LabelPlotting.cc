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

		colour_ = auto_ptr<Colour>(new Colour(grid.colour()));
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
