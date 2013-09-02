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

/*! \file SeaShading.cc
    \brief Implementation of the Template class SeaShading.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 7-Mar-2005
    
    Changes:
    
*/



#include "SeaShading.h"
#include "Transformation.h"
#include "SceneVisitor.h"
#include "Polyline.h"

using namespace magics;

SeaShading::SeaShading() 
{
}

SeaShading::~SeaShading() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void SeaShading::print(ostream& out)  const
{
	out << "SeaShading[";
	out << "]";
}

void SeaShading::sea(Polyline* sea, BasicGraphicsObjectContainer& visitor) 
{
	const Transformation& transformation = visitor.transformation();
	sea->setFilled(true);	
	sea->setFillColour(*colour_);
	sea->setShading(new FillShadingProperties());	
    double minx = transformation.getMinPCX();
    double miny = transformation.getMinPCY();
    double maxx = transformation.getMaxPCX();
    double maxy = transformation.getMaxPCY();
   // if (miny < -90) miny = -90;
   // if (maxy > 90)  maxy = 90;
    
	sea->push_back(PaperPoint(minx, miny));
	sea->push_back(PaperPoint(minx, maxy));
	sea->push_back(PaperPoint(maxx, maxy));
	sea->push_back(PaperPoint(maxx, miny));
	sea->push_back(PaperPoint(minx, miny));
	visitor.push_back(sea);
}

