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

/*! \file AxisControl.cc
    \brief Implementation of the Template class AxisControl.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 13-Oct-2005
    
    Changes:
    
*/


#include "AxisControl.h"
#include "Transformation.h"
#include "AxisMethod.h"

using namespace magics;

AxisControl::AxisControl() 
{
}


AxisControl::~AxisControl() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AxisControl::print(ostream& out)  const
{
	out << "AxisControl[";
	out << "]";
}

void AutomaticAxisControl::horizontal(Layout& layout, Transformation& transformation, AxisMethod& method)
{
    transformation.adjustXAxis(layout);
    method.updateX(transformation);
}


void AutomaticAxisControl::vertical(Layout& layout, Transformation& transformation, AxisMethod& method)
{
    transformation.adjustYAxis(layout);
    method.updateY(transformation);  
}

void AxisControl::vertical(Layout& , Transformation& transformation, AxisMethod& method)
{
    method.updateY(transformation);  
}

void AxisControl::horizontal(Layout& , Transformation& transformation, AxisMethod& method)
{
    method.updateX(transformation);
}

