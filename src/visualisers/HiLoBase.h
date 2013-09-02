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

/*! \file HiLoBase.h
    \brief Definition of the Template class HiLoBase.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 9-Feb-2006
    
    Changes:
    
*/

#ifndef HiLoBase_H
#define HiLoBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"

#include "BasicSceneObject.h"



namespace magics {

class XmlNode;
class MatrixHandler;

class HiLoBase {

public:
	HiLoBase() {}
	virtual ~HiLoBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "HiLoBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "HiLoBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual HiLoBase* clone() const { return 0; }
    virtual bool accept(const string&) { return false; }

    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "HiLoBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }    
    virtual BasicGraphicsObject* next() { 
    	MagLog::dev() << "HiLoBase::next()---> to be checked!...\n";
    	return 0; 
    }
    virtual bool more() { 
    	MagLog::dev() << "HiLoBase:: more(---> to be checked!...\n";
    	return false; 
    }
    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) {}
    virtual void visit(LegendVisitor&) {
    	MagLog::dev() << "HiLoBase::visit(LegendBase&)---> to be checked!...\n";
    }
    virtual void clear() {}
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "HiLoBase\n"; } 

private:
    //! Copy constructor - No copy allowed
	HiLoBase(const HiLoBase&);
    //! Overloaded << operator to copy - No copy allowed
	HiLoBase& operator=(const HiLoBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HiLoBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, HiLoBase > {
public:
	HiLoBase* operator()(const string& val )
	{
		return SimpleObjectMaker<HiLoBase >::create(val);
	}     

	HiLoBase* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};

} // namespace magics
#endif
