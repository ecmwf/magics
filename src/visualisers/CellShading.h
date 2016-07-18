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

/*! \file CellShading.h
    \brief Definition of the Template class CellShading.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 30-Aug-2005
    
    Changes:
    
*/

#ifndef CellShading_H
#define CellShading_H

#include "magics.h"

#include "IsoShading.h"
#include "CellShadingAttributes.h"

#include "ColourTable.h"

namespace magics {


class CellShading: public ShadingTechnique, public CellShadingAttributes {

public:
	CellShading();
	virtual ~CellShading();
	void set(const map<string, string>& map ) { CellShadingAttributes::set(map); }
	void set(const XmlNode& node ) { CellShadingAttributes::set(node); }
	bool accept(const string& node) { return CellShadingAttributes::accept(node); }

	virtual ShadingTechnique* clone() const {
		CellShading* object = new CellShading();
	    return object;
	}

	void copy(const CellShading& from)
	{
		CellShadingAttributes::copy(from);
		//IsoShadingAttributes::copy(from);
	}
	bool shadingMode() { return shading_ == "grid"; }
	virtual void visit(LegendVisitor&, const ColourTechnique&);

	int index(double value);
	void operator()(Polyline*) const;

	bool hasLegend() { return true; } // Isolien legend is not needed!
	virtual void operator()(IsoPlot*, MatrixHandler&, BasicGraphicsObjectContainer&);
    virtual bool prepare(const LevelSelection&, const ColourTechnique&);
    virtual void colour(double, Colour&);
    CellArray* array(MatrixHandler& matrix, IntervalMap<int>& range,
    		const Transformation& transformation, int width, int height,
    		float resolution, const string& technique);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 IntervalMap<int> map_;
	 vector<Colour>   colours_;
	 string 		  shading_;
	 bool 			  adaptive_;

private:
    //! Copy constructor - No copy allowed
	CellShading(const CellShading&);
    //! Overloaded << operator to copy - No copy allowed
	CellShading& operator=(const CellShading&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const CellShading& p)
		{ p.print(s); return s; }

};
class DumpShading: public CellShading {

public:
	DumpShading();
	virtual ~DumpShading();

	virtual ShadingTechnique* clone() const {
		DumpShading* object = new DumpShading();
	    return object;
	}
	void copy(const CellShading& from)
	{
		CellShadingAttributes::copy(from);
		//IsoShadingAttributes::copy(from);
	}


	void operator()(IsoPlot*, MatrixHandler&, BasicGraphicsObjectContainer&);

};





} // namespace magics


#endif
