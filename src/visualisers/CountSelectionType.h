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

/*! \file CountSelectionType.h
    \brief Definition of the Template class CountSelectionType.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
     
*/

#ifndef CountSelectionType_H
#define CountSelectionType_H

#include "magics.h"

#include "CountSelectionTypeAttributes.h"
#include "LevelSelection.h"
#include "XmlNode.h"

namespace magics {


class CountSelectionType: public CountSelectionTypeAttributes, public LevelSelection {

public:
	CountSelectionType();
	virtual ~CountSelectionType();



    virtual void calculate(double min, double max, bool);
    
    void set(const map<string, string>& params) { 
        CountSelectionTypeAttributes::set(params);
        LevelSelection::set(params);
    }
    void set(const XmlNode& node) { 
        CountSelectionTypeAttributes::set(node);
        if ( magCompare(node.name(), "count") ) {
        	XmlNode level = node;
        	level.name("level");
        	LevelSelection::set(level);
        }
    }
    void set(const LevelSelectionInterface&);

    double reference() const { return reference_; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	CountSelectionType(const CountSelectionType&);
    //! Overloaded << operator to copy - No copy allowed
	CountSelectionType& operator=(const CountSelectionType&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CountSelectionType& p)
		{ p.print(s); return s; }

};



} // namespace magics
#endif
