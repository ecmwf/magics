/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
	 map<double, PaperPoint> pressureRightLabels_;
	 map<double, PaperPoint> pressureLeftLabels_;
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
