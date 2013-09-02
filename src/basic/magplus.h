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


#include "magics.h"
#include "MagException.h"
#include "ParameterManager.h"
#include "MagicsParameter.h"
#include "OutputHandler.h"
#include "DriverManager.h"
#include "DisplayManager.h"

namespace magics {

class FortranRootSceneNode;
template <class P> class ActionNode;
template <class P> class Data;
template <class P> class Visdef;

class UserPoint;
class UserPoint;
class Axis;



struct MagType
{
	MagType() : content_(0) {}
	
	template <class T>
	MagType(const string& param, T val) {	
			content_ = ParameterManager::getCopy(param);		
			if ( content_ ) content_->set(val);
	}

	template <class T>
	void operator=(T val) {
		       if ( content_ ) content_->set(val);
	}
	void operator=(const char* val) {
		       if ( content_ ) content_->set(string(val));
	}
	
	BaseParameter* content_;

	friend ostream& operator<<(ostream& s, const MagType& p)
			{ if ( p.content_ ) s << *p.content_; return s; }
};



struct MagGlobalType
{
	MagGlobalType()  {}
	
	template <class T>
	MagGlobalType(T val) {
		ParameterManager::set(param_, val);
	}

	template <class T>
	void operator=(T val) {
		ParameterManager::set(param_, val);
	}
	void operator=(const char* val) {
		ParameterManager::set(param_, val);				     
	}
	
	string param_;
	
	friend ostream& operator<<(ostream& s, const MagGlobalType& p)
			{  s << p.param_;  return s; }
};


class MagDef: public map<string, MagType> {
	
public:
	MagDef()   {}
	virtual ~MagDef() {}
	MagType& operator()(const string&);

	template <class T>
	void operator()(const string&, const T&);

	void set() const;
	void reset() const;

protected:
	virtual void print(ostream&) const;
	vector<string> keys_;

private:
	//! Overloaded << operator to copy - No copy allowed
	MagDef& operator=(const MagDef&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s, const MagDef& p)
		{ p.print(s); return s; }
};


class Magics;
enum MagContext { GEOGRAPHICAL, CARTESIAN };


class MagAction: public MagDef {
	
public:
	MagAction() {}
	virtual ~MagAction() {}
	
	void action(Magics&) const;
	virtual void  build(Magics&) const  { MagLog::info() << *this << " is not yet implemented" << endl; }
	
protected:
	void print(ostream&) const {}
};


class MagCoastlines : public MagAction 
{
public:
	MagCoastlines();
    
protected:
	void print(ostream&) const;
	void build(Magics&) const;
};


class MagGrib : public MagAction 
{
public:
	MagGrib();
    
protected:
	void print(ostream&) const;
	void build(Magics&) const;
};


class MagContour : public MagAction
{
public:
	MagContour();
	    
protected:
	void print(ostream&) const;
	void build(Magics&) const;
};
	



class  Magics: public map<string, MagGlobalType> {

public:
	Magics();
	~Magics();
	void execute();

	template <class T> 
	void operator()(const string&, const T&);
   
	MagGlobalType& operator()(const string&);
    
	void add(const MagAction&);

	void pushTop(BasicSceneObject* object);
    	
	void node(BasicSceneObject* object);
	template <class P>  void visdef(Visdef<P>*);
	template <class P>  void data(Data<P>*);

	void context(MagContext context) { context_ = context; }
	MagContext context() const { return context_; }

protected:
	void print(ostream&) const;
    
	void drivers();
	void page();
	void superpage();
	void subpage();
	void actions();

	typedef void (Magics::*Action)();
  
	DriverManager*			drivers_;
	FortranRootSceneNode*		root_;
	OutputHandler*			output_;
	stack<Action>			actions_;
	stack<Axis*>			axis_;
	MagContext			context_;
	BasicSceneObject*		node_;
	stack<BasicSceneObject*>	stack_;

private:
	//! Copy constructor - No copy allowed
	Magics(const Magics&);
	//! Overloaded << operator to copy - No copy allowed
	Magics& operator=(const Magics&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s, const Magics& p)
		{ p.print(s); return s; }
};

} // end namespace
