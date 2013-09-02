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

/*! \file UserDate.h
    \brief Definition of the Template class UserDate.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 10-Oct-2005
    
    Changes:
    
*/

#ifndef UserDate_H
#define UserDate_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class UserDate {

public:
	UserDate();
	UserDate(const string&);
	virtual ~UserDate();
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual UserDate* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new UserDate();
    }
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	UserDate(const UserDate&);
    //! Overloaded << operator to copy - No copy allowed
	UserDate& operator=(const UserDate&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const UserDate& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, UserDate> { 
public:
	UserDate* operator()(const string& val )
	{
		 return new UserDate(val);		
	}     

	UserDate* magics(const string& param)
	{
		UserDate* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
