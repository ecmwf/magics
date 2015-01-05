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

/*! \file ShadingTechnique.h
    \brief Definition of the Template class ShadingTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#ifndef ShadingTechnique_H
#define ShadingTechnique_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "ColourTechnique.h"


namespace magics {

class LevelSelection;
class BasicGraphicsObject;
class MatrixHandler;
class Data;
class BasicGraphicsObjectContainer;
class Polyline;
class CellArray;
class Transformation;
class ContourMethod;

class ShadingTechnique  {


public:
	ShadingTechnique() {}
	virtual ~ShadingTechnique() {}
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    void toxml(ostream&)  const {}
    virtual ShadingTechnique* clone() const { return 0; }
    virtual bool more() { return false; }
    virtual bool shadingMode() { return false; }
    virtual bool hasLegend() { return false; }
    virtual BasicGraphicsObject* next() { return 0; }
    virtual void operator()(Data&, BasicGraphicsObjectContainer&) {}
	virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) {}
    virtual void operator()(Polyline*) const {};
    virtual bool prepare(const LevelSelection&, const ColourTechnique&)  { return false; }   
    virtual void visit(LegendVisitor&, const ColourTechnique&) {}   
    virtual int index(double) { assert(0); return -1; }
    virtual int leftIndex(double) { assert(0); return -1; }
    virtual int rightIndex(double) { assert(0); return -1; }
    virtual CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range,
       		const Transformation& transformation, int width, int height,
       		float resolution, const string& technique) { assert(0); }
    virtual bool needClipping() { return false; }
    virtual bool method(ContourMethod*) { return false; }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
	ShadingTechnique(const ShadingTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	ShadingTechnique& operator=(const ShadingTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ShadingTechnique& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, ShadingTechnique > {
public:
	ShadingTechnique* operator()(const string& val ) {
		 return SimpleObjectMaker<ShadingTechnique >::create(val);
	}     
    ShadingTechnique* magics(const string& param)
    {
        ShadingTechnique* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};
} // namespace magics

#endif
