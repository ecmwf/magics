/*! \file MagnifierVisitor.h
    \brief Definition of the Template class NoMagnifierVisitor.
    
    Magics Team - ECMWF 2009
    
    Started: Tue 27-Jan-2009
    
    Changes:
    
*/

#ifndef MagnifierVisitor_H
#define MagnifierVisitor_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "SceneVisitor.h"
#include "Layout.h"
#include "Symbol.h"
#include "PaperPoint.h"
#include "MagnifierVisitorAttributes.h"

namespace magics {



class NoMagnifierVisitor: public SceneVisitor, public MagnifierLayout{

public:
	NoMagnifierVisitor();
	virtual ~NoMagnifierVisitor();
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false;}
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual void toxml(ostream&, int = 0) const {}
    virtual NoMagnifierVisitor* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new NoMagnifierVisitor();
    }
    void visit(magics::BasicSceneObject&) {}
    virtual void add(const PaperPoint&) {}
    virtual void owner(BasicSceneObject* owner) { owner_ = owner; } 
   
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 BasicSceneObject* owner_;
	
	 
private:
    //! Copy constructor - No copy allowed
	NoMagnifierVisitor(const NoMagnifierVisitor&);
    //! Overloaded << operator to copy - No copy allowed
	NoMagnifierVisitor& operator=(const NoMagnifierVisitor&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoMagnifierVisitor& p)
		{ p.print(s); return s; }

};

class MagnifierVisitor : public NoMagnifierVisitor, public MagnifierVisitorAttributes
{
public:
	MagnifierVisitor();

	virtual ~MagnifierVisitor(); 
	void visit(BasicGraphicsObjectContainer&);  
	virtual NoMagnifierVisitor* clone() const {
		MagnifierVisitor* visitor = new MagnifierVisitor();
		visitor->owner_ = owner_;
		visitor->copy(*this);
		return visitor;
	} 
	virtual void set(const XmlNode& node) {
		MagnifierVisitorAttributes::set(node);
	}
	virtual void set(const map<string, string>& map) {
		MagnifierVisitorAttributes::set(map);
	}  
	void redisplay(const BaseDriver& driver) const;
	void redisplay(const BaseDriver& driver, vector<PaperPoint>&,float,float) const;
	void visit(magics::BasicSceneObject& object) { object.visit(*this); }
	void add(const PaperPoint&);
	void addMore(const PaperPoint&);
	

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;
	 mutable TextSymbol* values_;
	 mutable Symbol* more_;
	 
};

template <>
class MagTranslator<string, NoMagnifierVisitor> { 
public:
	NoMagnifierVisitor* operator()(const string& val )
	{
		return SimpleObjectMaker<NoMagnifierVisitor>::create(val);
	}     

	NoMagnifierVisitor* magics(const string& param)
	{
		NoMagnifierVisitor* object;
		ParameterManager::update(param, object);
		return object;
	}
};

} // namespace magics
#endif
