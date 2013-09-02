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

/*! \file LinearTableMode.h
    \brief Definition of the Template class LinearTableMode.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 14-July-2005
    
    Changes:
    
*/

#ifndef LinearTableMode_H
#define LinearTableMode_H

#include "magics.h"

#include "LookupTableMode.h"

namespace magics {

class LinearTableMode: public LookupTableMode {

public:
	LinearTableMode();
	virtual ~LinearTableMode();
	void set(const XmlNode& node ) // for MagML
			 { LookupTableModeAttributes::set(node); }
	void set(const map<string, string>& map ) // for MagML
		 { LookupTableModeAttributes::set(map); }

	virtual void operator()(Image&, Raster&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	LinearTableMode(const LinearTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	LinearTableMode& operator=(const LinearTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LinearTableMode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
