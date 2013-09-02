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

/*! \file Dimension.h
    \brief Implementation of the Template class Dimension.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 29-Mar-2004
    
    Changes:
    
*/

#include "Dimension.h"
#include "MagLog.h"

using namespace magics;

template<class T>
inline void helper(const string& in, T& out)
{
	std::stringstream is(in);
	is >> out;
}

Dimension::Dimension(const string& value, double parent, double def)
{
	if ( magCompare(value, "undef") ) {
		percent_ = def;
		absolute_ = parent*def*0.01;
		return;		
	}
	// Look for % ..
	string::size_type pos = value.find("%");
	if ( pos == 0 ) {
		MagLog::error() << "format(" << pos << ") is not valid\n";
		percent_ = def;
		absolute_ = parent*def*0.01;
		return;
	}		
	if (pos != string::npos) {		
		helper(value, percent_);
		absolute_ = parent*percent_*0.01;
		return;
	}

	// By default it is cm...
	helper(value, absolute_);
	percent_ = absolute_*100/parent;
}
