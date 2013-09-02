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

/*! \file EquidistantTableMode.h
    \brief Definition of the Template class EquidistantTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 17-May-2005
    
    Changes:
    
*/

#ifndef EquidistantTableMode_H
#define EquidistantTableMode_H

#include "magics.h"

#include "LookupTableMode.h"
#include "EquidistantTableModeAttributes.h"

#define ETM_MLEN 1024 //maximum number of elements of a histogram

namespace magics {

//class EquidistantTableMode: public LookupTableMode, public EquidistantTableModeAttributes {
class EquidistantTableMode: public LookupTableMode {

public:
	EquidistantTableMode();
	virtual ~EquidistantTableMode();
	void set(const map<string, string>& ) // for MagML
	{
	   //LookupTableMode::set(map); 
	   //EquidistantTableModeAttributes::set(map);
	}

	virtual void operator()(Image&, Raster&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	EquidistantTableMode(const EquidistantTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	EquidistantTableMode& operator=(const EquidistantTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const EquidistantTableMode& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
