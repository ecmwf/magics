/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HiLoMarkerBase.h
    \brief Definition of the Template class HiLoMarkerBase.
    
    Magics Team - ECMWF 2006
    
    Started: Fri 10-Feb-2006
    
    Changes:
    
*/

#ifndef HiLoMarkerBase_H
#define HiLoMarkerBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class XmlNode;
class HiLo;


class HiLoMarkerBase {
	// act like NoHiloMarker!

public:
	HiLoMarkerBase() {}
	virtual ~HiLoMarkerBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "HiLoMarkerBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "HiLoMarkerBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false; }
    virtual HiLoMarkerBase* clone() const {
        MagLog::dev() << "HiLoMarkerBase::set(const map<string, string&)---> to be checked!...\n";
        return new HiLoMarkerBase();
    }
    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "HiLoMarkerBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }  
    
   
    virtual void operator()(const PaperPoint&, HiLo&) {
    	
    	
    }
   
    virtual void clear()  {}
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "HiLoMarkerBase:same as NoHiloMarker\n"; } 

private:
    //! Copy constructor - No copy allowed
	HiLoMarkerBase(const HiLoMarkerBase&);
    //! Overloaded << operator to copy - No copy allowed
	HiLoMarkerBase& operator=(const HiLoMarkerBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoMarkerBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, HiLoMarkerBase > {
public:
	HiLoMarkerBase* operator()(const string& val )
	{
		return SimpleObjectMaker<HiLoMarkerBase >::create(val);
	}     

	HiLoMarkerBase* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};

} // namespace magics
#endif
