/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file PolyShadingMethod.h
    \brief Definition of the Template class PolyShadingMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#ifndef PolyShadingMethod_H
#define PolyShadingMethod_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "Polyline.h"
#include "ColourTechnique.h"
#include "LegendVisitor.h"
namespace magics {

class LevelSelection;
class PolyShadingMethod {

public:
	PolyShadingMethod() {}
	virtual ~PolyShadingMethod() {}    
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual bool accept(const string& node) { return magCompare(node, "area_fill"); }
	
	virtual void toxml(ostream&) {}
	virtual PolyShadingMethod* clone() const
	{
		PolyShadingMethod* object = new PolyShadingMethod();
		return object;
	}

	virtual bool shadingMode() { return true; }
	virtual int index(double);
	virtual int rightIndex(double);
	virtual int leftIndex(double);
	virtual void prepare(LevelSelection&, const ColourTechnique&);

    virtual void operator()(Polyline& poly) const;

    virtual void visit(LegendVisitor& legend, const ColourTechnique& colour);
    IntervalMap<int> indexes_;
    vector<Colour> colours_;
    double first_;
    double last_;
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
	PolyShadingMethod(const PolyShadingMethod&);
    //! Overloaded << operator to copy - No copy allowed
	PolyShadingMethod& operator=(const PolyShadingMethod&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PolyShadingMethod& p)
		{ p.print(s); return s; }

};




template <>
class MagTranslator<string, PolyShadingMethod > {
public:
	PolyShadingMethod* operator()(const string& val ) {
		 return SimpleObjectMaker<PolyShadingMethod >::create(val);
	}     
    PolyShadingMethod* magics(const string& param)
    {
        PolyShadingMethod* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};

} // namespace magics

#endif
