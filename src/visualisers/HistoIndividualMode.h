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

/*! \file HistoIndividualMode.h
    \brief Definition of the Template class HistoIndividualMode.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 18-May-2004
    
    Changes:
    
*/

#ifndef HistoIndividualMode_H
#define HistoIndividualMode_H

#include "magics.h"

#include "HistoMode.h"
#include "HistoIndividualModeAttributes.h"

namespace magics {

class Box;

class HistoIndividualMode: public HistoMode, public HistoIndividualModeAttributes {

public:
	HistoIndividualMode();
	virtual ~HistoIndividualMode();
    
    void set(const map<string, string>& map ) { HistoIndividualModeAttributes::set(map); }
    void set(const XmlNode& node) { HistoIndividualModeAttributes::set(node); }
    HistoMode* clone() const {
    	HistoIndividualMode* object;
    	object->copy(*this);
    	return object;
    }
    
    virtual void count(double, double);
    virtual void setToFirst(Layout&);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     Box* addButton(int var, int val, Layout&);

     map<double, int> map_;

private:
    //! Copy constructor - No copy allowed
	HistoIndividualMode(const HistoIndividualMode&);
    //! Overloaded << operator to copy - No copy allowed
	HistoIndividualMode& operator=(const HistoIndividualMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HistoIndividualMode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
