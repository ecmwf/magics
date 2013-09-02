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

/*! \file HeightSelectionList.h
    \brief Definition of the Template class HeightSelectionList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef HeightSelectionList_H
#define HeightSelectionList_H

#include "magics.h"

#include "HeightSelection.h"
#include "HeightSelectionListAttributes.h"

namespace magics {

class HeightSelectionList: public HeightSelection, public HeightSelectionListAttributes {

public:
	HeightSelectionList();
	virtual ~HeightSelectionList();
    virtual void set(map<string,string> map) { HeightSelectionListAttributes::set(map); }
    void prepare();
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	HeightSelectionList(const HeightSelectionList&);
    //! Overloaded << operator to copy - No copy allowed
	HeightSelectionList& operator=(const HeightSelectionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HeightSelectionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
