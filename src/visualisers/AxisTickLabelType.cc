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

/*! \file AxisTickLabelType.cc
    \brief Implementation of the Template class AxisTickLabelType.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 7-Oct-2005
    
    Changes:
    
*/



#include "AxisTickLabelType.h"
#include "PaperPoint.h"
#include "Text.h"
#include "MagicsFormat.h"

using namespace magics;

void AxisTickLabelType::print(ostream& out) const
{
	out << "AxisTickLabelType[]";
}

void AxisTickLabelType::toxml(ostream&)  const
{
}

void LabelListLabelType::toxml(ostream&, int)  const
{
}
void NumberLabelType::toxml(ostream&, int)  const
{
}

void LabelListLabelType::print(ostream& out) const
{
	out << "AxisTickLabelType[]";
}
	
void NumberLabelType::print(ostream& out) const
{
	out << "AxisTickLabelType[]";
}
	
LabelListLabelType::LabelListLabelType()
{
	current_ = labels_.begin();
}

string LabelListLabelType::label(const string& label)
{ 
	
	string nice = label;
	// first try to find in the cach!
	map<string, string>::iterator cache = cache_.find(label);
	if (cache != cache_.end()  )
		return cache->second;
	
	if (current_ != labels_.end() ) {
		nice = *current_;
		
		current_++;
	}
	cache_.insert(make_pair(label, nice));
	return nice;  	
} 


bool alldigits(const string& name)
{
	for (string::const_iterator c = name.begin(); c != name.end(); ++c)
		if ( !isdigit(*c) && *c != '.' && *c != '-' && *c != 'e' && *c != 'E' ) return false;
	return true;
} 

string NumberLabelType::label(const string& label)
{
	
	if ( label.empty() ) return label;
	if ( alldigits(label) == false ) return label;
	
	ostringstream nice;
    nice << MagicsFormat(format_, tonumber(label)); 
  
	return nice.str();
}

