/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HeightSelectionList.h
    \brief Definition of the Template class HeightSelectionList.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef HeightSelectionList_H
#define HeightSelectionList_H

#include "magics.h"

#include "HeightSelection.h"
#include "HeightSelectionListAttributes.h"

namespace magics {

class HeightSelectionList: public HeightSelection, public HeightSelectionListAttributes {

public:
	HeightSelectionList();
	virtual ~HeightSelectionList();
    virtual void set(map<string,string> map) { HeightSelectionListAttributes::set(map); }
    void prepare();
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	HeightSelectionList(const HeightSelectionList&);
    //! Overloaded << operator to copy - No copy allowed
	HeightSelectionList& operator=(const HeightSelectionList&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HeightSelectionList& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
