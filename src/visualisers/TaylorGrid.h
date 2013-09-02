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

/*! \file TaylorGrid.h
    \brief Definition of the Template class TaylorGrid.
    
    Magics Team - ECMWF 2006
    
    Started: Tue 3-Oct-2006
    
    Changes:
    
*/

#ifndef TaylorGrid_H
#define TaylorGrid_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "TaylorGridAttributes.h"

namespace magics {

class TaylorGrid:   public BasicSceneObject, public TaylorGridAttributes {

public:
	TaylorGrid();
	virtual ~TaylorGrid();
	
	// New Interface!
   void visit(DrawingVisitor& list);
   void secondary(DrawingVisitor& list);
   void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors);

   
	void set(const map<string, string>& map ) { TaylorGridAttributes::set(map); }
	void set(const XmlNode& node ) { TaylorGridAttributes::set(node); }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 void list(double ref, double inc, std::set<double>& out) const;

private:
    //! Copy constructor - No copy allowed
	TaylorGrid(const TaylorGrid&);
    //! Overloaded << operator to copy - No copy allowed
	TaylorGrid& operator=(const TaylorGrid&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const TaylorGrid& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
