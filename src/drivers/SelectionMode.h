
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
 
/*! \file SelectionMode.h
    \brief Definition of the Template class SelectionMode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 20-Nov-2007
    
    Changes:
    
*/

#ifndef SelectionMode_H
#define SelectionMode_H

#include "magics.h"

#include "SelectionModeAttributes.h"

namespace magics {

class SelectionMode: public SelectionModeAttributes {

public:
	SelectionMode();
	virtual ~SelectionMode();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	SelectionMode(const SelectionMode&);
    //! Overloaded << operator to copy - No copy allowed
	SelectionMode& operator=(const SelectionMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SelectionMode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
