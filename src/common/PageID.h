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

/*! \file PageID.h
    \brief Definition of the Template class PageID.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 29-Mar-2004
    
    Changes:
    
*/

#ifndef PageID_H
#define PageID_H

#include "magics.h"
#include "PageIDAttributes.h"
#include "BasicSceneObject.h"


namespace magics {



class NoPageID: public BasicSceneObject {

public:
	NoPageID() {}
	virtual ~NoPageID() {}

    virtual void set(const XmlNode&) {
        MagLog::dev() << "PageIDBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "PageIDBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) {
    	return false;
    }
    virtual NoPageID* clone() const {
        MagLog::dev() << "NoPageID::set(const map<string, string&)---> to be checked!...\n";
        return new NoPageID();
    }
    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "NoPageID::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }
    virtual bool needLegend() { return false; }
    virtual void visit(BasicGraphicsObjectContainer& ) {}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "NoPageID\n"; } 

private:
    //! Copy constructor - No copy allowed
	NoPageID(const NoPageID&);
    //! Overloaded << operator to copy - No copy allowed
	NoPageID& operator=(const NoPageID&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoPageID& p)
		{ p.print(s); return s; }

};

class PageID: public NoPageID, public PageIDAttributes {

public:
	PageID();
	virtual ~PageID();
	virtual void set(const map<string, string>& map) 
		{ PageIDAttributes::set(map); }
    virtual void set(const XmlNode& node) 
		{ PageIDAttributes::set(node); }
	 virtual bool accept(const string& node) 
	    { return PageIDAttributes::accept(node); }
	virtual void visit(BasicGraphicsObjectContainer& list);
	virtual NoPageID* clone() const;

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;

private:
	//! Copy constructor - No copy allowed
	PageID(const PageID&);
	//! Overloaded << operator to copy - No copy allowed
	PageID& operator=(const PageID&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PageID& p)
		{ p.print(s); return s; }
};


template <>
class MagTranslator<string, NoPageID>
{
public:
	NoPageID* operator()(const string& val )
	{
		return SimpleObjectMaker<NoPageID>::create(val);
	}

	NoPageID* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};

} // namespace magics
#endif
