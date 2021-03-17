/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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

class TaylorGrid : public BasicSceneObject, public TaylorGridAttributes {
public:
    TaylorGrid();
    virtual ~TaylorGrid() override;

    // New Interface!
    void visit(DrawingVisitor& list) override;
    void secondary(DrawingVisitor& list);
    void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors) override;


    void set(const map<string, string>& map) override { TaylorGridAttributes::set(map); }
    void set(const XmlNode& node) override { TaylorGridAttributes::set(node); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    void list(double ref, double inc, std::set<double>& out) const;

private:
    //! Copy constructor - No copy allowed
    TaylorGrid(const TaylorGrid&);
    //! Overloaded << operator to copy - No copy allowed
    TaylorGrid& operator=(const TaylorGrid&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TaylorGrid& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
