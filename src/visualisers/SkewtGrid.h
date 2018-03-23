/*
 * (C) Copyright 1996-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef SQEWTGRID_H
#define SQEWTGRID_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "TephiGridAttributes.h"

namespace magics {

class SkewtGrid:   public BasicSceneObject, public TephiGridAttributes {

public:
    SkewtGrid();
    virtual ~SkewtGrid();

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
    SkewtGrid(const SkewtGrid&);
    //! Overloaded << operator to copy - No copy allowed
    SkewtGrid& operator=(const SkewtGrid&);

// -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s,const SkewtGrid& p)
        { p.print(s); return s; }

};

} // namespace magics

#endif // SQEWTGRID_H
