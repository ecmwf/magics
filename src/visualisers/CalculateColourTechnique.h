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

/*! \file CalculateColourTechnique.h
    \brief Definition of the Template class CalculateColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#ifndef CalculateColourTechnique_H
#define CalculateColourTechnique_H

#include "magics.h"

#include "ColourTechnique.h"
#include "CalculateColourTechniqueAttributes.h"

namespace magics {

class CalculateColourTechnique: public ColourTechnique, public CalculateColourTechniqueAttributes {

public:
	CalculateColourTechnique();
	virtual ~CalculateColourTechnique();
    void set(const map<string, string>& map) { 
        CalculateColourTechniqueAttributes::set(map);
        }
    void set(const XmlNode& node) { 
        CalculateColourTechniqueAttributes::set(node); 
    }
     bool accept(const string& node) { return CalculateColourTechniqueAttributes::accept(node); }
    
    void set(const ColourTechniqueInterface&);
    
    
    virtual ColourTechnique* clone() const {
    	CalculateColourTechnique* object = new CalculateColourTechnique();
    	object->copy(*this);
    	return object;
    }
    const Colour& getMax() const { return *max_; }
    const Colour& getMin() const { return *min_; }
    const string& getDirection() const { return direction_; }

protected:
     void set(ColourTable&, int) const;
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	CalculateColourTechnique(const CalculateColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	CalculateColourTechnique& operator=(const CalculateColourTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CalculateColourTechnique& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
