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

/*! \file Title.h
    \brief Definition of the Template class Title.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 12-Feb-2004
    
    Changes:
    
*/

#ifndef Title_H
#define Title_H

#include "magics.h"
#include "TitleAttributes.h"
#include "TitleBase.h"



namespace magics {

class Node;



class Title: public TitleBase, public TitleAttributes {

public:
	Title();
	virtual ~Title();

	virtual TitleBase* clone() const {
		Title* object = new Title();
		object->copy(*this);
		return object;
	}
	
    virtual void set(const XmlNode& node) {
        TitleAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
        TitleAttributes::set(map);
    }
	virtual void add(const string& info) {
		if (empty()) { push_back(new TitleEntry()); }
		back()->add(info);
	}
	virtual void newline() {
		push_back(new TitleEntry());
	}
	virtual bool on() { return true; }
	void push_back(TitleEntry* entry) 
	   { vector<TitleEntry*>::push_back(entry); }
	MagFont font() const;
	virtual Justification justification() const { return justification_; }
	virtual string colour() const { return colour_->name(); }
	
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	Title(const Title&);
	//! Overloaded << operator to copy - No copy allowed
	Title& operator=(const Title&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Title& p)
		{ p.print(s); return s; }

};


class NoTitle: public TitleBase {

public:
	NoTitle() {}
	virtual ~NoTitle() {}
	virtual TitleBase* clone() const {
		TitleBase* object = new NoTitle();
		return object;
	}
	virtual void add(const string& ){
	}
	virtual void newline() {
	}
	virtual bool on() { return false; }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { out << "No Automatic Title"; } 

private:
	//! Copy constructor - No copy allowed
	NoTitle(const NoTitle&);
	//! Overloaded << operator to copy - No copy allowed
	NoTitle& operator=(const NoTitle&);
};


} // namespace magics
#endif
