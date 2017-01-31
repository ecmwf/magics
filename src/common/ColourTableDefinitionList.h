/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionList.h
    \brief Definition of the Template class ColourTableDefinitionList.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/

#ifndef ColourTableDefinitionList_H
#define ColourTableDefinitionList_H

#include "magics.h"

#include "ColourTableDefinition.h"
#include "ColourTableDefinitionListInterface.h"


namespace magics {

class LevelSelection;

class ColourTableDefinitionList: public ColourTableDefinition {

public:
	ColourTableDefinitionList();
	virtual ~ColourTableDefinitionList();
	void set(const ColourTableDefinitionListInterface&);
	void set(const XmlNode&);
	void set(ColourTable&, int);

    ColourTableDefinition* clone() const {
		ColourTableDefinitionList* object = new ColourTableDefinitionList();
		object->colours_ = colours_;
		return object;
	}
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 mutable stringarray colours_;
	 mutable ListPolicy policy_;

private:
    //! Copy constructor - No copy allowed
	ColourTableDefinitionList(const ColourTableDefinitionList&);
    //! Overloaded << operator to copy - No copy allowed
	ColourTableDefinitionList& operator=(const ColourTableDefinitionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourTableDefinitionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
