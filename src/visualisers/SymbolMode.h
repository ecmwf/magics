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

/*! \file SymbolMode.h
    \brief Definition of the Template class SymbolMode.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 21-Jan-2004
    
    Changes:
    
*/

#ifndef SymbolMode_H
#define SymbolMode_H

#include "magics.h"

#include "SymbolIndividualModeAttributes.h"
#include "SymbolModeAttributes.h"
#include "SymbolTableModeAttributes.h"

#include "MagTranslator.h"
#include "Factory.h"
#include "Colour.h"
#include "IntervalMap.h"
#include "Symbol.h"
#include "UserPoint.h"
#include "UserPoint.h"
#include "Text.h"
#include "Data.h"

namespace magics {
	
class LegendVisitor;
class HistoVisitor;
class SymbolPlotting;


class SymbolMode : public SymbolModeAttributes
{

public:
	SymbolMode();
	virtual ~SymbolMode();
	
	virtual SymbolMode* clone() const {
		SymbolMode* object = new SymbolMode();
		return object;
	}
   
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    virtual void toxml(ostream&, int = 0) const {}
    
    virtual bool accept(double) { return true; }   
    virtual void prepare() {}
	virtual  SymbolProperties operator()(double) const { throw OutOfRangeMagException(); }
	
    void parent(SymbolPlotting* parent) { parent_ = parent; }

    
    virtual void visit(LegendVisitor&) {};
    virtual void visit(Data&, LegendVisitor& legend) { visit(legend); }
    virtual void visit(Data&, HistoVisitor&);

	//virtual void adjust(double min, double max) {}
	virtual void adjust(double , double ) {}
	void set(const string& type) { type_ = type; }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 SymbolPlotting* parent_;
	 string type_;
private:
	//! Copy constructor - No copy allowed
	SymbolMode(const SymbolMode&);
	//! Overloaded << operator to copy - No copy allowed
	SymbolMode& operator=(const SymbolMode&);


// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SymbolMode& p)
		{ p.print(s); return s; }
};

class SymbolIndividualMode: public SymbolMode, public SymbolIndividualModeAttributes {

public:
	SymbolIndividualMode();
	virtual ~SymbolIndividualMode();
	virtual void set(const map<string, string>& map) {
		SymbolMode::set(map);
		SymbolIndividualModeAttributes::set(map);
		update();
		
	}
	virtual void set(const XmlNode& node) {
		SymbolMode::set(node);
		SymbolIndividualModeAttributes::set(node);
		update();
    
	}

	virtual bool accept(const string& node) {
		return SymbolIndividualModeAttributes::accept(node);
    
	}
	
	virtual SymbolMode* clone() const {
		SymbolIndividualMode* object = new SymbolIndividualMode();
		//SymbolIndividualModeAttributes::copy(*this);
		return object;
	}
   
    
    virtual void visit(LegendVisitor&);
	void prepare() { update(); }
    void update();
    void properties() const;
    SymbolProperties operator()(double) const {  properties(); return properties_; }
    void visit(Data&, LegendVisitor&);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     mutable SymbolProperties properties_;
     mutable vector<string>::const_iterator current_;


private:
    //! Copy constructor - No copy allowed
	SymbolIndividualMode(const SymbolIndividualMode&);
    //! Overloaded << operator to copy - No copy allowed
	SymbolIndividualMode& operator=(const SymbolIndividualMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SymbolIndividualMode& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, SymbolMode> { 
public:
	SymbolMode* operator()(const string& val ) {
		 return SimpleObjectMaker<SymbolMode>::create(val);
	}
   
	SymbolMode* magics(const string& param)
	{
		SymbolMode* object;
		ParameterManager::update(param, object);
		return object;
	}

};


class SymbolTableMode: public SymbolMode, public SymbolTableModeAttributes {

public:
	SymbolTableMode();
	virtual ~SymbolTableMode();
    virtual void prepare();
    virtual bool accept(double);
    SymbolProperties operator()(double) const;

    void set(const map<string, string>& map ) { 
        SymbolTableModeAttributes::set(map);
        SymbolMode::set(map);
        prepare();
    }
    
    virtual void set(const XmlNode& node) {
		SymbolMode::set(node);
		SymbolTableModeAttributes::set(node);
		prepare();
    
	}
    virtual bool accept(const string& node) {
		return SymbolTableModeAttributes::accept(node);
    
	}
    
    void visit(LegendVisitor&); 
    void visit(Data&, LegendVisitor&);
    void visit(Data&, HistoVisitor&);
    void buildBins(const IntervalMap<SymbolProperties>&, IntervalMap<Colour>&); 

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     IntervalMap<SymbolProperties>  map_;
private:
    //! Copy constructor - No copy allowed
	SymbolTableMode(const SymbolTableMode&);
    //! Overloaded << operator to copy - No copy allowed
	SymbolTableMode& operator=(const SymbolTableMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SymbolTableMode& p)
		{ p.print(s); return s; }

};


} // namespace magics

#endif
