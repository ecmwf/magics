/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LevelListSelectionType.h
    \brief Definition of the Template class LevelListSelectionType.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 10-Mar-2004
    
    Changes:
    
*/

#ifndef LevelListSelectionType_H
#define LevelListSelectionType_H

#include "magics.h"

#include "LevelListSelectionTypeAttributes.h"
#include "GradientsSelectionTypeAttributes.h"
#include "LevelSelection.h"

namespace magics {

class LevelListSelectionType: public LevelListSelectionTypeAttributes, public LevelSelection {

public:
	LevelListSelectionType();
	virtual ~LevelListSelectionType();

   
    void calculate(double min, double max, bool); 
    void set(const map<string, string>& params) { 
        LevelListSelectionTypeAttributes::set(params);
        LevelSelection::set(params);
    }
    void set(const XmlNode& node) { 
        LevelListSelectionTypeAttributes::set(node);
        LevelSelection::set(node);
    }
    void set(const LevelSelectionInterface& from) {
        list_ = from.getList();
    }
    virtual LevelSelection* clone() const {
    	LevelListSelectionType* object = new LevelListSelectionType();
    	object->copy(*this);
    	return object;
    }

    double reference(int) const { return  empty() ? -9999:    front(); }

    void copy(const LevelListSelectionType& from) {
    	 LevelListSelectionTypeAttributes::copy(from);
         LevelSelection::copy(from);
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	LevelListSelectionType(const LevelListSelectionType&);
    //! Overloaded << operator to copy - No copy allowed
	LevelListSelectionType& operator=(const LevelListSelectionType&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LevelListSelectionType& p)
		{ p.print(s); return s; }

};


class GradientsSelectionType: public GradientsSelectionTypeAttributes, public LevelSelection {

public:
    GradientsSelectionType() {}
    virtual ~GradientsSelectionType() {}

   
    void calculate(double min, double max, bool); 
    void set(const map<string, string>& params) { 
        GradientsSelectionTypeAttributes::set(params);
        LevelSelection::set(params);
    }
    void set(const XmlNode& node) { 
        GradientsSelectionTypeAttributes::set(node);
        LevelSelection::set(node);
    }
    void set(const LevelSelectionInterface& from) {
        list_ = from.getList();
    }
    virtual LevelSelection* clone() const {
        GradientsSelectionType* object = new GradientsSelectionType();
        object->copy(*this);
        return object;
    }

    double reference(int) const { return  empty() ? -9999:    front(); }

    void copy(const GradientsSelectionType& from) {
         GradientsSelectionTypeAttributes::copy(from);
         LevelSelection::copy(from);
    }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
     virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
    GradientsSelectionType(const GradientsSelectionType&);
    //! Overloaded << operator to copy - No copy allowed
    GradientsSelectionType& operator=(const GradientsSelectionType&);

// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const GradientsSelectionType& p)
        { p.print(s); return s; }

};


} // namespace magics
#endif
