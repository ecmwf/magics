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

/*! \file BackgroundColour.h
    \brief Definition of Colour class.
    \author Meteorological Visualisation Section, ECMWF

    Started 2002

*/
#ifndef BackgroundColour_H
#define BackgroundColour_H


#include "Colour.h"
#include "Factory.h"
#include "MagTranslator.h"


namespace magics {

class BackgroundColour : public Colour
{
public:
	BackgroundColour() {}
	BackgroundColour(const string& colour) : Colour(colour) {}
	BackgroundColour* clone() const { return new BackgroundColour(*this); }
};

template<>
class MagTranslator<string, BackgroundColour> { 
public:
	BackgroundColour* operator()(const string& val )
	{
		 if (Colour::valid(val) ) return new BackgroundColour(val);
		 if ( val == "background_colour" ) return new BackgroundColour();
		 throw NoFactoryException(val);
	}

	BackgroundColour* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics

#endif
