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

/*! \file LevelListSelectionType.cc
    \brief Implementation of the Template class LevelListSelectionType.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 10-Mar-2004
    
    Changes:
    
*/



#include "LevelListSelectionType.h"

using namespace magics;

LevelListSelectionType::LevelListSelectionType() 
{
}


LevelListSelectionType::~LevelListSelectionType() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void LevelListSelectionType::print(ostream& out)  const
{
	out << "LevelListSelectionType[";
	LevelSelection::print(out);
	LevelListSelectionTypeAttributes::print(out);
	out << "]";
}


void LevelListSelectionType::calculate(double , double, bool) 
{
	clear();
	


	for (doublearray::const_iterator val = list_.begin(); val != list_.end(); ++val) {
		MagLog::dev() << "LevelListSelectionType::calculate(double min, double max)--->" << *val << "\n";
		if ( min_ <= *val && *val <= max_)
			push_back(*val);
	}

	
	ostringstream print;
	print <<  "LevelListSelectionType::calculate-->";
	string sep = "[";
	for (vector<double>::const_iterator val = begin(); val != end(); ++val) {
		print << sep << *val;
		sep = ", ";
	}
	print << "]";
	MagLog::dev() << print.str() << endl;
}

