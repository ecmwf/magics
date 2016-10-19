/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourSelectionList.h
    \brief Definition of the Template class ColourSelectionList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef ColourSelectionList_H
#define ColourSelectionList_H

#include "magics.h"

#include "ColourSelection.h"
#include "ColourSelectionListAttributes.h"

namespace magics {

class ColourSelectionList: public ColourSelection, public ColourSelectionListAttributes {

public:
	ColourSelectionList();
	virtual ~ColourSelectionList();
    virtual void set(map<string,string> map) { ColourSelectionListAttributes::set(map); }
    void prepare();

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	ColourSelectionList(const ColourSelectionList&);
    //! Overloaded << operator to copy - No copy allowed
	ColourSelectionList& operator=(const ColourSelectionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourSelectionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
