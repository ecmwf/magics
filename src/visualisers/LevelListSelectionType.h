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

/*! \file LevelListSelectionType.h
    \brief Definition of the Template class LevelListSelectionType.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 10-Mar-2004
    
    Changes:
    
*/

#ifndef LevelListSelectionType_H
#define LevelListSelectionType_H

#include "magics.h"

#include "LevelListSelectionTypeAttributes.h"
#include "LevelSelection.h"

namespace magics {

class LevelListSelectionType: public LevelListSelectionTypeAttributes, public LevelSelection {

public:
	LevelListSelectionType();
	virtual ~LevelListSelectionType();

   
    void calculate(double min, double max, bool); 
    void set(const map<string, string>& params) { 
        LevelListSelectionTypeAttributes::set(params);
        LevelSelection::set(params);
    }
    void set(const XmlNode& node) { 
        LevelListSelectionTypeAttributes::set(node);
        LevelSelection::set(node);
    }
    void set(const LevelSelectionInterface& from) {
        list_ = from.getList();
    }
    virtual LevelSelection* clone() const {
    	LevelListSelectionType* object = new LevelListSelectionType();
    	object->copy(*this);
    	return object;
    }

    double reference(int) const { return  empty() ? -9999:    front(); }

    void copy(const LevelListSelectionType& from) {
    	 LevelListSelectionTypeAttributes::copy(from);
         LevelSelection::copy(from);
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	LevelListSelectionType(const LevelListSelectionType&);
    //! Overloaded << operator to copy - No copy allowed
	LevelListSelectionType& operator=(const LevelListSelectionType&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LevelListSelectionType& p)
		{ p.print(s); return s; }

};




} // namespace magics
#endif
