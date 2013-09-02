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

/*! \file Wind.h
    \brief Definition of the Template class Wind.
    
    Magics Team - ECMWF 2005
    
    Started: Thu 17-Mar-2005
    
    Changes:
    
*/

#ifndef Wind_H
#define Wind_H

#include "magics.h"

#include "Visdef.h"
#include "WindAttributes.h"
#include "ThinningMethod.h"

namespace magics {


class Wind: public Visdef, public WindAttributes, public ThinningMethodUI {

public:
	Wind();
	virtual ~Wind();
	
	virtual void set(const map<string, string>& map) 
		{ WindAttributes::set(map); }
	virtual void set(const XmlNode& node) 
		{ WindAttributes::set(node); }
		
	// implements the Visdef interface ...
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    bool needLegend() { return type_->needLegend(); }
    void getReady(const LegendVisitor& legend) { Visdef::getReady(legend); type_->legendOnly(legendOnly_); }
    void  visit(Data& data, HistoVisitor& visitor);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 

private:
    //! Copy constructor - No copy allowed
	Wind(const Wind&);
    //! Overloaded << operator to copy - No copy allowed
	Wind& operator=(const Wind&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Wind& p)
		{ p.print(s); return s; }

};



} // namespace magics


#endif
