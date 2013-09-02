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

/*! \file Contour.h
    \brief Definition of the Template class Contour.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 3-Mar-2004
    
    Changes:
    
*/

#ifndef Contour_H
#define Contour_H

#include "magics.h"

#include "ContourAttributes.h"
#include "Visdef.h"

namespace magics {



class Contour: public ContourAttributes, public Visdef {

public:
   
	Contour();
	virtual ~Contour();
	
	virtual Contour* clone() const {
		Contour* contour = new Contour();
		contour->copy(*this);
		return contour;
	}
	bool needLegend() { return legend_; }
	// Implements the set method ... 
	void set(const XmlNode& node)                  { ContourAttributes::set(node); }
	void set(const map<string, string>& map) { ContourAttributes::set(map); }
	
    // Implements the VisDefinterface 
   virtual void operator()(Data&, BasicGraphicsObjectContainer&);
   virtual void visit(Data&, LegendVisitor&);
    
   void visit(Data&, HistoVisitor&);
   void  getReady(const LegendVisitor& legend) { contour_->legend_only_ = legend.only_; }
    

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 MatrixHandler* matrix_;

private:
    //! Copy constructor - No copy allowed
	Contour(const Contour&);
    //! Overloaded << operator to copy - No copy allowed
	Contour& operator=(const Contour&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const Contour& p)
		{ p.print(s); return s; }

};

} // namespace magics

#endif
