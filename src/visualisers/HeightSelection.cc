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

/*! \file HeightSelection.cc
    \brief Implementation of the Template class HeightSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/



#include "HeightSelection.h"

using namespace magics;

HeightSelection::HeightSelection() 
{
}


HeightSelection::~HeightSelection() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void HeightSelection::print(ostream& out)  const
{
	out << "HeightSelection[";
	out << "]";
}

