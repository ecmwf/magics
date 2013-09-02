
/*******************************  LICENSE  *******************************


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


 *******************************  LICENSE  *******************************/
/*! \file SelectionMode.cc
    \brief Implementation of the Template class SelectionMode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 20-Nov-2007
    
    Changes:
    
*/



#include "SelectionMode.h"

using namespace magics;

SelectionMode::SelectionMode() 
{
}


SelectionMode::~SelectionMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void SelectionMode::print(ostream& out)  const
{
	out << "SelectionMode[";
	out << "]";
}

