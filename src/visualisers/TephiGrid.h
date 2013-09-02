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

/*! \file TephiGrid.h
    \brief Definition of the Template class TephiGrid.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 3-Oct-2006
    
    Changes:
    
*/

#ifndef TephiGrid_H
#define TephiGrid_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "TephiGridAttributes.h"

namespace magics {

class TephiGrid:   public BasicSceneObject, public TephiGridAttributes {

public:
	TephiGrid();
	virtual ~TephiGrid();
	
	// New Interface!
   void visit(DrawingVisitor&);
   void visit(LeftAxisVisitor&);
   void visit(RightAxisVisitor&);
   void visit(BottomAxisVisitor&);
   void visit(TopAxisVisitor&);

   void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors);

   
	void set(const map<string, string>& map ) { TephiGridAttributes::set(map); }
	void set(const XmlNode& node ) { TephiGridAttributes::set(node); }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 map<double, PaperPoint> pressureLabels_;
	 map<double, PaperPoint> mixingLabels_;
	 map<double, PaperPoint> isothermLabels_;
	 map<double, PaperPoint> isothetaLabels_;
	 map<double, PaperPoint> satLabels_;

private:
    //! Copy constructor - No copy allowed
	TephiGrid(const TephiGrid&);
    //! Overloaded << operator to copy - No copy allowed
	TephiGrid& operator=(const TephiGrid&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TephiGrid& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
