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

/*! \file MarkerSelectionList.h
    \brief Definition of the Template class MarkerSelectionList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef MarkerSelectionList_H
#define MarkerSelectionList_H

#include "magics.h"

#include "MarkerSelection.h"
#include "MarkerSelectionListAttributes.h"

namespace magics {

class MarkerSelectionList: public MarkerSelection, public MarkerSelectionListAttributes {

public:
	MarkerSelectionList();
	virtual ~MarkerSelectionList();
    virtual void set(map<string,string> map) { MarkerSelectionListAttributes::set(map); }
    void prepare();
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	MarkerSelectionList(const MarkerSelectionList&);
    //! Overloaded << operator to copy - No copy allowed
	MarkerSelectionList& operator=(const MarkerSelectionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MarkerSelectionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
