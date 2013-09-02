/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
