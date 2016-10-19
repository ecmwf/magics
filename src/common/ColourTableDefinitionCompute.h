/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionCompute.h
    \brief Definition of the Template class ColourTableDefinitionCompute.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#ifndef ColourTableDefinitionCompute_H
#define ColourTableDefinitionCompute_H

#include "magics.h"

#include "ColourTableDefinition.h"
#include "ColourTableDefinitionComputeInterface.h"

namespace magics {

class ColourTableDefinitionCompute: public ColourTableDefinition {

public:
	ColourTableDefinitionCompute();
	virtual ~ColourTableDefinitionCompute();
	void set(const ColourTableDefinitionComputeInterface&);
	void set(const XmlNode&);
	ColourTableDefinition* clone() const {
		ColourTableDefinitionCompute* object = new ColourTableDefinitionCompute();
		object->minColour_ = minColour_;
		object->maxColour_ = maxColour_;
		object->direction_ = direction_;
		return object;
	}
	void set(ColourTable&, int);
	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 Colour     minColour_;
	 Colour     maxColour_;
	 string  direction_;
private:
    //! Copy constructor - No copy allowed
	ColourTableDefinitionCompute(const ColourTableDefinitionCompute&);
    //! Overloaded << operator to copy - No copy allowed
	ColourTableDefinitionCompute& operator=(const ColourTableDefinitionCompute&);
	
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourTableDefinitionCompute& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
