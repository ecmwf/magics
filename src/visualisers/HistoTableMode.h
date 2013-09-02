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

/*! \file HistoTableMode.h
    \brief Definition of the Template class HistoTableMode.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 18-May-2004
    
    Changes:
    
*/

#ifndef HistoTableMode_H
#define HistoTableMode_H

#include "magics.h"

#include "HistoMode.h"
#include "HistoTableModeAttributes.h"

namespace magics {

class HistoTableMode: public HistoMode, public HistoTableModeAttributes {

public:
	HistoTableMode();
	virtual ~HistoTableMode();
    void set(const map<string, string>& map ) { HistoTableModeAttributes::set(map); }
       void set(const XmlNode& node) { HistoTableModeAttributes::set(node); }
    HistoMode* clone() const {
    	HistoTableMode* object;
    	object->copy(*this);
    	return object;
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	HistoTableMode(const HistoTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	HistoTableMode& operator=(const HistoTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HistoTableMode& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
