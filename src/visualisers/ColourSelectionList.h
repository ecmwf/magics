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

/*! \file ColourSelectionList.h
    \brief Definition of the Template class ColourSelectionList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef ColourSelectionList_H
#define ColourSelectionList_H

#include "magics.h"

#include "ColourSelection.h"
#include "ColourSelectionListAttributes.h"

namespace magics {

class ColourSelectionList: public ColourSelection, public ColourSelectionListAttributes {

public:
	ColourSelectionList();
	virtual ~ColourSelectionList();
    virtual void set(map<string,string> map) { ColourSelectionListAttributes::set(map); }
    void prepare();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	ColourSelectionList(const ColourSelectionList&);
    //! Overloaded << operator to copy - No copy allowed
	ColourSelectionList& operator=(const ColourSelectionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourSelectionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
