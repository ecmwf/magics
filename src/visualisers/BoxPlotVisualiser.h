/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BoxPlotVisualiser.h
    \brief Definition of the Template class BoxPlotVisualiser.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef BoxPlotVisualiser_H
#define BoxPlotVisualiser_H

#include "magics.h"


#include "BoxPlotVisualiserAttributes.h"


#include "Graph.h"
#include "Polyline.h"
#include "magics.h"

namespace magics {

class XmlNode;
class LegendVisitor;

class BoxPlotVisualiser : public BoxPlotVisualiserAttributes, public Visdef {
public:
    BoxPlotVisualiser();
    virtual ~BoxPlotVisualiser();


    void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);


    // Implements the set method ...
    void set(const map<string, string>& map) { BoxPlotVisualiserAttributes::set(map); }
    void set(const XmlNode& node) { BoxPlotVisualiserAttributes::set(node); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    double resolution_;


private:
    //! Copy constructor - No copy allowed
    BoxPlotVisualiser(const BoxPlotVisualiser&);
    //! Overloaded << operator to copy - No copy allowed
    BoxPlotVisualiser& operator=(const BoxPlotVisualiser&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BoxPlotVisualiser& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
