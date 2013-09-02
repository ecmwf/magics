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
 /*
    \brief Implementation of the Template class HourDateAxis.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/



#include "HourDateAxis.h"

using namespace magics;

HourDateAxis::HourDateAxis() 
{
}


HourDateAxis::~HourDateAxis() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void HourDateAxis::print(ostream& out)  const
{
	out << "HourDateAxis[";
	out << "]";
}

void HourDateAxis::label(AxisItem& item) const
{
	item.format("%H h");
	item.height(height_);
}
