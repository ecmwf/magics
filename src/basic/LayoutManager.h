/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LayoutManager.h
    \brief Definition of the Template class LayoutManager.
    
    Magics Team - ECMWF 2009
    
    Started: Mon 19-Jan-2009
    
    Changes:
    
*/

#ifndef LayoutManager_H
#define LayoutManager_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class BasicSceneNode; 
class BasicPositionalObject; 

class LayoutManager {

public:
	LayoutManager();
	virtual ~LayoutManager();
	
	virtual LayoutManager* clone() { return new LayoutManager(); }
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual LayoutManager* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new LayoutManager();
    }
    
    void newpage() { newpage_ = true; }
    
    virtual BasicSceneNode* operator()(BasicSceneNode* parent, BasicPositionalObject* node);
    
    static LayoutManager* manager(const string& type, const string& start, const string& direction);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	bool newpage_; 

private:
    //! Copy constructor - No copy allowed
	LayoutManager(const LayoutManager&);
    //! Overloaded << operator to copy - No copy allowed
	LayoutManager& operator=(const LayoutManager&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LayoutManager& p)
		{ p.print(s); return s; }

};

class AutomaticLayout : public LayoutManager
{
public:
	 	AutomaticLayout(): x_(0), y_(0), gapx_(0), gapy_(0) {}
		~ AutomaticLayout() {}
	
		void gapx(double gapx) { gapx_ = gapx; }
		void gapy(double gapy) { gapx_ = gapy; }
		
protected:
	   double x_;
	   double y_;
	   double gapx_;
	   double gapy_;
};

class MagMLLayoutManager : public AutomaticLayout
{
public:
	    MagMLLayoutManager() ;
		~ MagMLLayoutManager() {}
		BasicSceneNode* operator()(BasicSceneNode* parent, BasicPositionalObject* node);
		virtual LayoutManager* clone() { 
			MagMLLayoutManager* layout = new MagMLLayoutManager(); 
					
					return layout;
								
				}
		
protected:
	    BasicSceneNode* block(BasicSceneNode* parent, BasicPositionalObject* node);
	    BasicSceneNode* inline_display(BasicSceneNode* parent, BasicPositionalObject* node);
	    BasicSceneNode* absolute(BasicSceneNode* parent, BasicPositionalObject* node);
	    
		typedef BasicSceneNode* (MagMLLayoutManager::*Action)(BasicSceneNode* parent, BasicPositionalObject* node);
		static std::map<DisplayType, Action> actions_;
		double maxy_;
		double maxx_;
};

class BottomVerticalLayoutManager : public AutomaticLayout
{
public:
	BottomVerticalLayoutManager();
		~ BottomVerticalLayoutManager();
		BasicSceneNode* operator()(BasicSceneNode* parent, BasicPositionalObject* node);
		virtual LayoutManager* clone() { 
			BottomVerticalLayoutManager* layout = new BottomVerticalLayoutManager(); 
			layout->gapx_ = gapx_;
			layout->gapy_ = gapy_;
			return layout;
						
		}
};

class BottomHorizontalLayoutManager : public AutomaticLayout
{
public:
	BottomHorizontalLayoutManager();
		~ BottomHorizontalLayoutManager();
		BasicSceneNode* operator()(BasicSceneNode* parent, BasicPositionalObject* node);
		virtual LayoutManager* clone() { 
			BottomHorizontalLayoutManager* layout = new BottomHorizontalLayoutManager(); 
			layout->gapx_ = gapx_;
			layout->gapy_ = gapy_;
			return layout;
						
		}
};

class TopHorizontalLayoutManager : public AutomaticLayout
{
public:
	TopHorizontalLayoutManager();
		~ TopHorizontalLayoutManager();
		BasicSceneNode* operator()(BasicSceneNode* parent, BasicPositionalObject* node);
		virtual LayoutManager* clone() { 
			TopHorizontalLayoutManager* layout = new TopHorizontalLayoutManager(); 
			layout->gapx_ = gapx_;
			layout->gapy_ = gapy_;
			return layout;
						
		}
};

class TopVerticalLayoutManager : public AutomaticLayout
{
public:
	TopVerticalLayoutManager();
		~ TopVerticalLayoutManager();
		BasicSceneNode* operator()(BasicSceneNode* parent, BasicPositionalObject* node);
		virtual LayoutManager* clone() { 
			TopVerticalLayoutManager* layout = new TopVerticalLayoutManager(); 
			layout->gapx_ = gapx_;
			layout->gapy_ = gapy_;
			return layout;
						
		}
};


template <>
class MagTranslator<string, LayoutManager> { 
public:
	LayoutManager* operator()(const string& val )
	{
		return SimpleObjectMaker<LayoutManager>::create(val);
	}     

	LayoutManager* magics(const string& param)
	{
		LayoutManager* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
