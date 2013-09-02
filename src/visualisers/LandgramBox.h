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

/*! \file LandgramBox.h
    \brief Definition of the Template class LandgramBox.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 5-May-2004
    
    Changes:
    
*/

#ifndef LandgramBox_H
#define LandgramBox_H

#include "magics.h"


#include "LandgramBoxAttributes.h"


#include "magics.h"
#include "UserPoint.h"
#include "Visdef.h"
namespace magics {

class XmlNode;
class LegendVisitor;

class LandgramBox: public LandgramBoxAttributes, public Visdef {



public:
	LandgramBox();
	virtual ~LandgramBox();
    
   
    
    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);
    
    
    // Implements the set method ... 
    void set(const map<string, string>& map ) { LandgramBoxAttributes::set(map); }
    void set(const XmlNode& node) { LandgramBoxAttributes::set(node); }
    
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 
 
    

private:
    //! Copy constructor - No copy allowed
	LandgramBox(const LandgramBox&);
    //! Overloaded << operator to copy - No copy allowed
	LandgramBox& operator=(const LandgramBox&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LandgramBox& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
