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

/*! \file TitleBase.h
    \brief Definition of the Template class TitleBase.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 9-Feb-2006
    
    Changes:
    
*/

#ifndef TitleBase_H
#define TitleBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "MagFont.h"


namespace magics {

class XmlNode;
class TitleEntry {
public:
    TitleEntry(const string entry = "") : entry_(entry) {}
    ~TitleEntry() {}
    string entry();
    string entry_;
    void add(const string& info ) {
        entry_ += info;
    }
};



class TitleBase :public vector<TitleEntry*>  {

public:
	TitleBase() {}
	virtual ~TitleBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "TitleBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "TitleBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual TitleBase* clone() const {
        MagLog::dev() << "TitleBase::set(const map<string, string&)---> to be checked!...\n";
        return new TitleBase();
    }
    virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "TitleBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    }  
    virtual void add(const string& info){
    	if (empty()) { push_back(new TitleEntry()); }
		back()->add(info);
	}
	virtual void newline() {
		MagLog::dev() << "TitleBase::newline()---> to be checked!...\n";
	}
	virtual bool on() { 
		MagLog::dev() << "TitleBase:: on()---> to be checked!...\n";
		return false; 
	}
    virtual MagFont font() const  { return MagFont(); }
    virtual Justification justification() const { return MLEFT; }
	virtual string colour() const { return "NONE"; }
	virtual void titles(vector<string>&) {}
	
	void update(const string&, const string&, const string&);
 	string get(const string&, const string&);
 	void reset() { definitions_.erase(definitions_.begin(), definitions_.end());}
 	
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "TitleBase\n"; } 
	 typedef map<string, std::set<string> > DefList;
     static map<string, DefList > definitions_;

private:
    //! Copy constructor - No copy allowed
	TitleBase(const TitleBase&);
    //! Overloaded << operator to copy - No copy allowed
	TitleBase& operator=(const TitleBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TitleBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, TitleBase> { 
public:
	TitleBase* operator()(const string& val )
	{
		return SimpleObjectMaker<TitleBase>::create(val);
	}     

	TitleBase* magics(const string& param)
	{
		TitleBase* object=0;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
