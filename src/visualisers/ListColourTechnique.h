/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
   
    virtual void set(LevelSelection&, LevelSelection&, ColourTable&, int) const;
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
