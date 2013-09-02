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

/*! \file PageIDBase.h
    \brief Definition of the Template class PageIDBase.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 9-Feb-2006
    
    Changes:
    
*/

#ifndef PageIDBase_H
#define PageIDBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class Task;


class PageIDBase {

public:
	PageIDBase() {}
	virtual ~PageIDBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "PageIDBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "PageIDBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual PageIDBase* clone() const {
        MagLog::dev() << "PageIDBase::set(const map<string, string&)---> to be checked!...\n";
        return new PageIDBase();
    }
    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "PageIDBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }  
    
    virtual void operator()(const ErrorReporter&, Task&) const {
    	MagLog::dev() << "PageIDBase::operator()(const ErrorReporter&, vector<BaseGraphicsObject*>&)---> to be checked!...\n";
    }
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "PageIDBase\n"; } 

private:
    //! Copy constructor - No copy allowed
	PageIDBase(const PageIDBase&);
    //! Overloaded << operator to copy - No copy allowed
	PageIDBase& operator=(const PageIDBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PageIDBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, PageIDBase> { 
public:
	PageIDBase* operator()(const string& val )
	{
		return SimpleObjectMaker<PageIDBase>::create(val);
	}     

	PageIDBase* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}
};

} // namespace magics
#endif
