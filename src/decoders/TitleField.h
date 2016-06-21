/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TitleField.h
    \brief Definition of the Template class TitleField.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/

#ifndef TitleField_H
#define TitleField_H

#include "magics.h"


namespace magics {

class TitleField : public map<string, string> {

public:
	TitleField();
	virtual ~TitleField();
    
    string attribute(const string& param, const string& def = "") 
    {
        map<string, string>::const_iterator attribute = find(param);
        if ( attribute == end() ) return def;
        return attribute->second;
    }
    virtual string name() { return ""; }
    virtual void operator()(vector<string>&) const =0;

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	TitleField(const TitleField&);
    //! Overloaded << operator to copy - No copy allowed
	TitleField& operator=(const TitleField&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TitleField& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
