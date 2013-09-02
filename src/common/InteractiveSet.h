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

/*! \file InteractiveSet.h
    \brief Implementation of InteractiveSet class.
    
    Magics Team - ECMWF 2004
    
    Started: March 2004
    
    Changes:
    
*/
#ifndef InteractiveSet_H
#define InteractiveSet_H

#include "BasicGraphicsObject.h"


namespace magics {

class InteractiveAction
{
public:
	InteractiveAction() {}
	virtual ~InteractiveAction() {}
	
	virtual void execute() {} //protype to be confirmed!
protected:
	virtual	void print(ostream&) const {}
	friend ostream& operator<<(ostream& s,const InteractiveAction& p)
		{ p.print(s); return s; }
	
};

class InteractiveBegin : public BasicGraphicsObject, public map<string, InteractiveAction*> 
{
public :
	InteractiveBegin(Layer* layer = 0, const string& box = "");
	~InteractiveBegin() {}	
	
	bool reproject(const Transformation& transformation, BasicGraphicsObjectContainer& out) const;
	
	virtual void redisplay(const BaseDriver&) const;
protected:
    virtual	void print(ostream&) const;
	
};

class InteractiveEnd : public BasicGraphicsObject
{
public :
	InteractiveEnd(Layer* layer = 0, const string& box = "");
	~InteractiveEnd() {}	
	
	virtual void redisplay(const BaseDriver&) const;	
	bool reproject(const Transformation& transformation, BasicGraphicsObjectContainer& out) const;
protected:
	virtual	void print(ostream&) const;
	
};


class InteractiveSet : public BasicGraphicsObject, public vector<BasicGraphicsObject* > {
public:
	InteractiveSet(Layer* layer = 0, const string& box = "");
	virtual ~InteractiveSet() {}

	virtual bool reproject(const Transformation&, BasicGraphicsObjectContainer&) const;		
	virtual void redisplay(const BaseDriver&) const;
	void addAction(const string& name, InteractiveAction* action) 
		 { begin_->insert(make_pair(name, action)); }

protected:
	virtual	void print(ostream&) const;
	Layer	*layer_;
	string	box_;
	mutable Label	*label_; 
	Colour	colour_;
	bool    toBeDeleted_;
	InteractiveBegin *begin_;
	InteractiveEnd *end_;
	
private:
// No copy allowed
	InteractiveSet& operator=(const InteractiveSet&);

// -- Friends
	friend ostream& operator<<(ostream& s,const InteractiveSet& p)
		{ p.print(s); return s; }
};




class InteractiveLink : public InteractiveAction
{
public:
	InteractiveLink(const string& url="http://www.ecmwf.int") : url_(url) {}
	~InteractiveLink() {}	
	void url(const string& url) { url_ = url; }
	const string& url() const { return url_; }
protected:
	string url_;
	virtual	void print(ostream&) const;
	
};

class InteractiveMagnify: public InteractiveAction
{
public:
	InteractiveMagnify(float factor = 2 ) : factor_(factor) {}
	~InteractiveMagnify() {}	
	void factor(float factor) { factor_ = factor; }
	float factor() const { return factor_; }
protected:
	float factor_;
	virtual	void print(ostream&) const;
	
};







} // namespace magics
#endif
