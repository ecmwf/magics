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

/*! \file Label.h
    \brief Implementation of the Template class Label.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 16-Mar-2004
    
    Changes:
    
*/

#include "Label.h"

using namespace magics;

Label::Label(const string& label) : label_(label),
	justification_(MCENTRE), verticalAlign_(MBASE),  angle_(0.), 
	blanking_(false), visible_(false) 
{
}



Label::Label(double label) : label_(tostring(label)),  
		justification_(MCENTRE), verticalAlign_(MBASE), angle_(0.), 
		blanking_(false), visible_(false) 
{

}

Label::~Label() 
{}

/*!
 Class information are given to the output-stream.
*/		
void Label::print(ostream& out)  const
{
	out << "Label[";
	out << "label = " << label_;
	out << ",visible = " << visible_;
	out << ",blanking = " << blanking_;
	out << ",font = " << font_;
	out << ",angle = " << angle_;
	out << "]";
}
