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

/*! \file Flag.cc
    \brief Implementation of the Template class Flag.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 16-Mar-2005
    
    Changes:
*/

#include "Flag.h"

using namespace magics;


Flag::Flag() : length_(1), convention_(SI)
{
}


Flag::~Flag() 
{
}

/*!
 Class information are given to the output-stream.
*/	
	
void Flag::print(ostream& out)  const
{
	out << "Flag";
}
	
