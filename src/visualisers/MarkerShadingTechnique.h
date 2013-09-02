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

/*! \file MarkerShadingTechnique.h
    \brief Definition of the Template class MarkerShadingTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 26-Aug-2004
    
    Changes:
    
*/

#ifndef MarkerShadingTechnique_H
#define MarkerShadingTechnique_H

#include "magics.h"

#include "ShadingTechnique.h"
#include "MarkerShadingTechniqueAttributes.h"
#include "Symbol.h"
#include "SymbolMode.h"

namespace magics {


class MarkerShadingTechnique: 
    private vector<BasicGraphicsObject*>, // Internal buffer to store graphical objects during creation phase
    public ShadingTechnique,
    public MarkerShadingTechniqueAttributes {

public:
	MarkerShadingTechnique();
	virtual ~MarkerShadingTechnique();
    virtual void set(const map<string, string>& map) 
        { MarkerShadingTechniqueAttributes::set(map); }
    virtual void set(const XmlNode& node) 
        { MarkerShadingTechniqueAttributes::set(node); }
    virtual ShadingTechnique* clone() const {
    	MarkerShadingTechnique* object = new MarkerShadingTechnique();
    	object->copy(*this);
    	return object;
    }
    
    bool accept(const string& node) { return MarkerShadingTechniqueAttributes::accept(node); }

    
    Symbol* operator()(double);
    virtual void operator()(const PaperPoint&);
    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&);
    virtual bool prepare(const LevelSelection&, const ColourTechnique&);    
    virtual void visit(LegendVisitor&, const ColourTechnique&);
    bool hasLegend() { return true; } // Isolien legend is not needed!
    CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range,
           		const Transformation& transformation, int width, int height,
           		float resolution, const string& technique);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     vector<Symbol* >::iterator current_;
     map<Interval, Symbol* > map_;
     map<Interval, Symbol* > legend_;

private:
    //! Copy constructor - No copy allowed
	MarkerShadingTechnique(const MarkerShadingTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	MarkerShadingTechnique& operator=(const MarkerShadingTechnique&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MarkerShadingTechnique& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
