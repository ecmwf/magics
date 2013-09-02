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

/*! \file ColourTableDefinitionList.h
    \brief Definition of the Template class ColourTableDefinitionList.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#ifndef ColourTableDefinitionList_H
#define ColourTableDefinitionList_H

#include "magics.h"

#include "ColourTableDefinition.h"
#include "ColourTableDefinitionListInterface.h"


namespace magics {

class ColourTableDefinitionList: public ColourTableDefinition {

public:
	ColourTableDefinitionList();
	virtual ~ColourTableDefinitionList();
	void set(const ColourTableDefinitionListInterface&);
	void set(const XmlNode&);
	void set(ColourTable&, int);

    ColourTableDefinition* clone() const {
		ColourTableDefinitionList* object = new ColourTableDefinitionList();
		object->colours_ = colours_;
		return object;
	}
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 mutable stringarray colours_;
	 mutable ListPolicy policy_;

private:
    //! Copy constructor - No copy allowed
	ColourTableDefinitionList(const ColourTableDefinitionList&);
    //! Overloaded << operator to copy - No copy allowed
	ColourTableDefinitionList& operator=(const ColourTableDefinitionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourTableDefinitionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
