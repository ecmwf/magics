/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
