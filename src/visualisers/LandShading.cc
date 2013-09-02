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




/*! \file LandShading.cc
    \brief Implementation of the Template class LandShading.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 7-Mar-2005
    
    Changes:
    
*/



#include "LandShading.h"
#include "CoastPlotting.h"

#include "UserPoint.h"

using namespace magics;

LandShading::LandShading()
{
}

LandShading::~LandShading() 
{
}

void LandShading::setBoundingBox(const Transformation& transformation)
{
}
/*
void LandShading::operator()(PolyCoast* coast) 
{
        //MagLog::dev()<< "level--->" << coast->level() <<  "[" << coast->area() << "]\n";
        coast->setFilled(true);
        FillShadingProperties* shading = new FillShadingProperties();        

        // here you have to make sure that the coast are always oreinted in the same direction: anticlockwise 
        coast->makeCounterClockwise();
        
	// check the level 
	switch (coast->level()) {
		case 0: // South Pole   
		{
			std::reverse(coast->begin(), coast->end());
			coast->setFillColour(*colour_);    			 				
			break;
		}
		case 1: // island
			coast->setFillColour(*colour_);
			coast->land(false);   		
			break;
		case 2: //lake      
			coast->setFillColour( (*sea_).colour() );          			
			break;
		case 3: // island in lake		
			coast->setFillColour( *colour_ );
			coast->land(true);   		
			break;
		case 4 : // lake in islanfd in lake!
			coast->setFillColour( (*sea_).colour() ); 
			break;
	}
        coast->setShading(shading);
}

*/
/*!
 Class information are given to the output-stream.
*/		
void LandShading::print(ostream& out)  const
{
	out << "LandShading[";
	out << "]";
}
