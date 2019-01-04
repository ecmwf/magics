/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HistoMode.h
    \brief Definition of the Template class HistoMode.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 18-May-2004
    
    Changes:
    
*/

#ifndef HistoMode_H
#define HistoMode_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "AutoVector.h"
#include "BasicGraphicsObject.h"


namespace magics {


class Layout;

class HistoMode {

public:
	HistoMode();
	virtual ~HistoMode();
    
    virtual void set(const map<string, string>&) = 0; 
    virtual void set(const XmlNode&) = 0; 
    virtual HistoMode* clone() const = 0;
     virtual void toxml(ostream&, int)  const {}
    virtual void count(double, double) {}
    virtual void setToFirst(Layout&) {}
    virtual BasicGraphicsObject* next()  { return (*(current_++)).get(); }
    virtual bool more()     		    { return current_ != objects_.end(); }
    string button(int);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     
    AutoVector<BasicGraphicsObject>			    objects_;
    AutoVector<BasicGraphicsObject>::iterator	current_;
    static int count_;

private:
    //! Copy constructor - No copy allowed
	HistoMode(const HistoMode&);
    //! Overloaded << operator to copy - No copy allowed
	HistoMode& operator=(const HistoMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HistoMode& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, HistoMode> { 
public:
	HistoMode* operator()(const string& val ) {
		 return SimpleObjectMaker<HistoMode>::create(val);
	}     
    HistoMode* magics(const string& param)
    {
        HistoMode* object;
		ParameterManager::update(param, object);
		return object;
    }
};


} // namespace magics
#endif
