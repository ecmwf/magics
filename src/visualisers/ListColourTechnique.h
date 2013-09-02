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

/*! \file ListColourTechnique.h
    \brief Definition of the Template class ListColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#ifndef ListColourTechnique_H
#define ListColourTechnique_H

#include "magics.h"

#include "ColourTechnique.h"
#include "ListColourTechniqueAttributes.h"
namespace magics {

class ListColourTechnique: public ColourTechnique, public ListColourTechniqueAttributes {

public:
	ListColourTechnique();
	virtual ~ListColourTechnique();
    void set(const map<string, string>& map) { 
        ListColourTechniqueAttributes::set(map); 
    }
    void set(const XmlNode& node) { 
        ListColourTechniqueAttributes::set(node); 
    }
    bool accept(const string& node) { return ListColourTechniqueAttributes::accept(node); }
    
    virtual ColourTechnique* clone() const {
    	ListColourTechnique* object = new ListColourTechnique();
    	object->copy(*this);
    	return object;
    }
   
    virtual void set(ColourTable&, int) const;
	void set(const ColourTechniqueInterface&);
	stringarray getValues() const { return values_; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

     
private:
    //! Copy constructor - No copy allowed
	ListColourTechnique(const ListColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	ListColourTechnique& operator=(const ListColourTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ListColourTechnique& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
