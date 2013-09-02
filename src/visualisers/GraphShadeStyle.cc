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

