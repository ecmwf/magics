/*! \file FrameLoop.h
    \brief Definition of the Template class FrameLoop.
    
    Magics Team - ECMWF 2008
    
    Started: Fri 29-Aug-2008
    
    Changes:
    
*/

#ifndef FrameLoop_H
#define FrameLoop_H

#include "magics.h"

#include "BasicGraphicsObject.h"
#include "BasicSceneObject.h"
#include "TagHandler.h"
#include "Factory.h"
namespace magics {

class AnimationRules;
class AsIsAnimationRules;

class TopAxisVisitor;
class BottomAxisVisitor;
class LeftAxisVisitor;
class RightAxisVisitor;



class AnimationStep : public map<Layer*, int>, public TagHandler
{
public:
	AnimationStep(AnimationRules&);
	virtual ~AnimationStep();

	void rules(vector<string>&);
	AnimationRules& rules_;
	const map<string, string>& label(); 
	void xResolution(double xres) { xResolution_ = std::min(xResolution_, xres); }
	void yResolution(double yres) { yResolution_ = std::min(yResolution_, yres); }
	double xResolution() const { return xResolution_; }
	double yResolution() const { return yResolution_; }
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	map<string, string>    label_;
	double       xResolution_;
	double       yResolution_;
		  	 
private:
	//! Copy constructor - No copy allowed
	AnimationStep(const AnimationStep&);
	//! Overloaded << operator to copy - No copy allowed
	AnimationStep& operator=(const AnimationStep&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AnimationStep& p)
	{ p.print(s); return s; }
};



class AnimationRules : public vector<AnimationStep*>
{
public:
	AnimationRules();
	virtual ~AnimationRules();
	virtual void add(StepLayer&);
	virtual void rules(vector<string>&) const;
	
protected:
	     //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	string labelFormat_;
	
private:
	//! Copy constructor - No copy allowed
	AnimationRules(const AnimationRules&);
	//! Overloaded << operator to copy - No copy allowed
	AnimationRules& operator=(const AnimationRules&);

	// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AnimationRules& p)
		{ p.print(s); return s; }
};


class AsIsAnimationRules : public AnimationRules
{
public:
	AsIsAnimationRules();
	virtual ~AsIsAnimationRules();
	
	void add(StepLayer&);
	void rules(vector<string>&) const;
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
};

class NoOverlayAnimationRules : public AnimationRules
{
public:
	NoOverlayAnimationRules();
	virtual ~NoOverlayAnimationRules();
	
	void add(StepLayer&);
	void rules(vector<string>&) const;
		
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
};


class  DateAnimationRules : public AnimationRules
{
public:
	DateAnimationRules();
	virtual ~DateAnimationRules();
	void add(StepLayer&);
	void rules(vector<string>&) const;

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const;
	map<DateDescription, AnimationStep* > steps_;
};

class  LevelAnimationRules : public AnimationRules
{
public:
	LevelAnimationRules();
	virtual ~LevelAnimationRules();
	void add(StepLayer&);
	void rules(vector<string>&) const;

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	map<LevelDescription, AnimationStep* > steps_;
};
template <>
class MagTranslator<string, AnimationRules > {
public:
	AnimationRules* operator()(const string& val) {
		return SimpleObjectMaker<AnimationRules >::create(val);
	}

	AnimationRules* magics(const string& param)
	{
		AnimationRules* object=0;
		ParameterManager::update(param, object);
		return object;
	}
};
} // namespace magics
#endif
