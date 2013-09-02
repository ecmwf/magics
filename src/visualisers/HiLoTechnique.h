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

/*! \file HiLoTechnique.h
    \brief Definition of the Template class HiLoTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 24-Jun-2004
    
    Changes:
    
*/

#ifndef HiLoTechnique_H
#define HiLoTechnique_H

#include "magics.h"
#include "HiLoTechniqueAttributes.h"



namespace magics {

class HiLo;


class HiLoTechnique: public HiLoTechniqueAttributes {

public:
	HiLoTechnique() {}
	virtual ~HiLoTechnique() {}
	virtual HiLoTechnique* clone() {
		HiLoTechnique* object = new HiLoTechnique();
		object->copy(*this);
	    return object;
	}
    virtual void operator()(const PaperPoint&, HiLo&) {}
    virtual void clear() {}
    void set(const map<string, string>& map) { HiLoTechniqueAttributes::set(map); }
	void set(const XmlNode& node) { HiLoTechniqueAttributes::set(node); }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
	HiLoTechnique(const HiLoTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	HiLoTechnique& operator=(const HiLoTechnique&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoTechnique& p)
		{ p.print(s); return s; }

};



template<>
class MagTranslator<string, HiLoTechnique > {
public:
	HiLoTechnique* operator()(const string& val ) {
		 return SimpleObjectMaker<HiLoTechnique >::create(val);
	}     
    HiLoTechnique* magics(const string& param)
    {
       	HiLoTechnique* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};


} // namespace magics

#endif
