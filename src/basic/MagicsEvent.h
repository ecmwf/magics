/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/*! \file MagicsEvent.h
    \brief Definition of the Template class MagicsEvent.
    
    Magics Team - ECMWF 2007
    
    Started: Thu 22-Nov-2007
    
    Changes:
    
*/

#ifndef MagicsEvent_H
#define MagicsEvent_H

#include "magics.h"
#include "MagicsObserver.h"


namespace magics {

class Layer;
class MetaDataCollector;
class DateTime;

class MagicsEvent
{
public:
	MagicsEvent() {}	
	virtual ~MagicsEvent() {}	
	virtual void notify(MagicsObserver& ) {}
	
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { out << "MagicsEvent[]"; } 

private:
	//! Copy constructor - No copy allowed
	MagicsEvent(const MagicsEvent&);
	//! Overloaded << operator to copy - No copy allowed
	MagicsEvent& operator=(const MagicsEvent&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MagicsEvent& p)
		{ p.print(s); return s; }
};

class MagicsCursorEvent : public MagicsEvent
{
public:
	MagicsCursorEvent(const string& cursor = "") : cursor_(cursor) {}	
	virtual ~MagicsCursorEvent() {}	
	virtual void notify(MagicsObserver& observer) { observer.notify(*this); }
	const string& cursor() { return cursor_; }
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { out << "MagicsCursorEvent[cursor : " << cursor_ << "]"; } 
	string cursor_;
};
class MagicsSwapBufferEvent : public MagicsEvent
{
public:
	MagicsSwapBufferEvent() {}	
	virtual ~MagicsSwapBufferEvent() {}	
	virtual void notify(MagicsObserver& observer) { observer.notify(*this); }
		
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { out << "MagicsSwapBufferEvent[]"; } 
		
};

class MagicsAntialiasingEvent: public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsAntialiasingEvent(bool set) : set_(set) {}	
	virtual ~MagicsAntialiasingEvent() {}	
	virtual void notify(MagicsObserver& observer) { observer.notify(*this); }
	bool set() const { return set_; }
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		string set = ( set_ ) ? "on" : "false";
		out << "MagicsAntialiasingEvent[" << set  << "]"; 
	} 
	bool set_;		
};

class MagicsZoomEvent:  public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsZoomEvent(bool set) : set_(set) {}	
	virtual ~MagicsZoomEvent() {}	
	void notify(MagicsObserver& observer) { observer.notify(*this); }
	bool set() const { return set_; }
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		string set = ( set_ ) ? "on" : "false";
		out << "MagicsZoomEvent[" << set  << "]"; 
	} 
	bool set_;		
};

class MagicsMagnifierEvent:  public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsMagnifierEvent(bool set) : set_(set) {}	
	virtual ~MagicsMagnifierEvent() {}	
	void notify(MagicsObserver& observer) { observer.notify(*this); }
	bool set() const { return set_; }
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		string set = ( set_ ) ? "on" : "false";
		out << "MagicsMagnifierEvent[" << set  << "]"; 
	} 
	bool set_;
		
};

class MagicsRestoreFbEvent:  public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsRestoreFbEvent() {}	
	virtual ~MagicsRestoreFbEvent() {}	
	void notify(MagicsObserver& observer) { observer.notify(*this); }
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		out << "MagicsRestoreFbEvent[" << "]"; }	
};

class MagicsAnimationStepData
{
public:
	MagicsAnimationStepData(vector<string>& label, bool /*cached*/) : label_(label), cached_(false) {};
	const vector<string>& label() const  {return label_;}
	bool cached() const {return cached_;}	
	void setLabel(vector<string>& s) {label_=s;}
	void setCached(bool b) {cached_=b;}
private:	
	vector<string> label_;
	bool           cached_;
};


class MagicsAnimationCurrentStepEvent:  public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsAnimationCurrentStepEvent(int step) : step_(step) {}	
	virtual ~MagicsAnimationCurrentStepEvent() {}	
	void notify(MagicsObserver& observer) { observer.notify(*this); }
	int step() const { return step_; }
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		out << "MagicsAnimationCurrentStepEvent[" << step_ << "]"; }	
	int step_;
};

class MagicsAnimationStepsEvent:  public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsAnimationStepsEvent(vector<MagicsAnimationStepData> steps) : steps_(steps) {}	
	virtual ~MagicsAnimationStepsEvent() {}	
	void notify(MagicsObserver& observer) { observer.notify(*this); }
	const vector<MagicsAnimationStepData>& steps() const { return steps_; }
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		out << "MagicsAnimationFramseEvent[" << "]"; }	
	vector<MagicsAnimationStepData>  steps_;
};


class MagicsLayerUpdateEvent:  public MagicsEvent
{
// From Uplot to driver!
public:
	MagicsLayerUpdateEvent() {}	
	virtual ~MagicsLayerUpdateEvent() {}	
	void notify(MagicsObserver& observer) { observer.notify(*this); }
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const { 
		out << "MagicsLayerUpdateEvent[" << "]"; }	
};

class LevelDescription
{
public:
	LevelDescription();
	~LevelDescription();
	static LevelDescription surface(int set, int index) {
		LevelDescription level;
		level.surface_ = true;
		level.index_ = index;
		level.set_ = set;
		return level;
	}
	static LevelDescription level(const string& unit, double l, int set, int index) {
		LevelDescription newlevel;
		newlevel.surface_ = false;
		newlevel.unit_ = unit;
		newlevel.level_ = l;
		newlevel.set_ = set;
		newlevel.index_ = index;

		return newlevel;
	}

	bool operator < (const LevelDescription& other) const; // Code function in Layer.cc
	void update(const LevelDescription&) const;
protected:
	bool surface_;
	string unit_;
	double level_;
	mutable int set_;
	mutable int index_;
};
class DateDescription
{
public:
	DateDescription() {}
	DateDescription(const string& valid, int set, int index) : valid_(valid), set_(set), index_(index) {}
	~DateDescription();


	void update(const DateDescription&) const;
	bool operator < (const DateDescription& other) const; // Code function in Layer.cc
protected:
	string valid_;
	mutable int set_;
	mutable int index_;
};
struct MetviewIcon
{
public:
	MetviewIcon(const string& name = "", const string& cname = "", const string& id="unknown") :
		iconName_(name), iconClass_(cname), iconId_(id), visibilityLayer_(true), zindexLayer_(-1), transparencyLayer_(0) {};
		
	void icon(const string& name, const string& cname, const string& id="unknown") {
		iconName_ = name;
		iconClass_ = cname;
		iconId_ = id;
	}

	void layerInfo(bool visibility, int zindex, int transparency, const string& id, const string& name) {
		visibilityLayer_ = visibility;
		zindexLayer_ = zindex;
		transparencyLayer_ = transparency;
		idLayer_ = id;
		nameLayer_ = name;
	}
	
	virtual void visit(Layer& layer);

	virtual void visit(MetaDataCollector& collector);

	
	virtual void initInfo() {}
	string info(const string& name) {return (information_.find(name) != information_.end())?information_[name]:"";}
	void setInfo(const string& name, const string& value) { information_[name]=value; }
	void icon(const MetviewIcon& other) {
		iconName_ = other.iconName_;
		iconClass_ = other.iconClass_;
		iconId_ = other.iconId_;
		visibilityLayer_ = other.visibilityLayer_;
  		zindexLayer_ = other.zindexLayer_;
  		transparencyLayer_ = other.transparencyLayer_;
		idLayer_ = other.idLayer_;
		nameLayer_ = other.nameLayer_;

	}	
	string iconName() const   { return iconName_;     }
    string iconClass() const  { return iconClass_;    }
    string iconId() const     { return iconId_;       }
    int zindex() const        { return zindexLayer_;       }
 	int visibility() const    { return visibilityLayer_;   } 
	int transparency() const  { return transparencyLayer_; }
	const string& id() const  { return idLayer_; }
	const string& name() const  { return nameLayer_; }

protected:
	string iconName_;
	string iconClass_;
	string iconId_;
	bool visibilityLayer_;
    int zindexLayer_;
    int transparencyLayer_;
    string idLayer_;
    string nameLayer_;
	
	map<string, string> information_;

};

} // namespace magics
#endif
