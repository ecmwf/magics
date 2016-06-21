/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file IntervalSelectionType.h
    \brief Definition of the Template class IntervalSelectionType.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
    
*/

#ifndef IntervalSelectionType_H
#define IntervalSelectionType_H

#include "magics.h"

#include "IntervalSelectionTypeAttributes.h"
#include "LevelSelection.h"

namespace magics {

class IntervalSelectionType: public IntervalSelectionTypeAttributes, public LevelSelection {

public:
	IntervalSelectionType();
	virtual ~IntervalSelectionType();
	void set(const map<string, string>& params) { 
        IntervalSelectionTypeAttributes::set(params);
        LevelSelection::set(params);
    }
    void set(const XmlNode& node) { 
        IntervalSelectionTypeAttributes::set(node);
        LevelSelection::set(node);
    }
    void set(const LevelSelectionInterface& from) { 
    	reference_ = from.getReference();
        interval_ = from.getInterval();
        min_ = from.getMin();
        max_ = from.getMax();
    }
    virtual LevelSelection* clone() const {
    	IntervalSelectionType* object = new IntervalSelectionType();
    	object->copy(*this);
    	return object;
    }
    void copy(const IntervalSelectionType& from) {
    	 IntervalSelectionTypeAttributes::copy(from);
         LevelSelection::copy(from);
    }

    void calculate(double min, double max, bool); 
    double reference(int) const;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	IntervalSelectionType(const IntervalSelectionType&);
    //! Overloaded << operator to copy - No copy allowed
	IntervalSelectionType& operator=(const IntervalSelectionType&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const IntervalSelectionType& p)
		{ p.print(s); return s; }

};



} // namespace magics
#endif
