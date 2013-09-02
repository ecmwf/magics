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

/*! \file NetcdfMarsConvention.h
    \brief Definition of the Template class NetcdfMarsConvention.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#ifndef NetcdfMarsConvention_H
#define NetcdfMarsConvention_H

#include "magics.h"

#include "NetcdfConvention.h"

namespace magics {

class NetcdfMarsConvention: public NetcdfConvention {

public:
	NetcdfMarsConvention();
	virtual ~NetcdfMarsConvention();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	NetcdfMarsConvention(const NetcdfMarsConvention&);
    //! Overloaded << operator to copy - No copy allowed
	NetcdfMarsConvention& operator=(const NetcdfMarsConvention&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NetcdfMarsConvention& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
