/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MetgramGraph.h
    \brief Definition of the Template class MetgramGraph.

    Magics Team - ECMWF 2004

    Started: Wed 5-May-2004

    Changes:

*/

#ifndef MetgramGraph_H
#define MetgramGraph_H

#include "magics.h"


#include "MetgramGraphAttributes.h"


#include "Graph.h"
#include "Polyline.h"
#include "magics.h"

namespace magics {

class XmlNode;


class MetgramGraph : public MetgramGraphAttributes, public Visdef {
public:
    MetgramGraph();
    virtual ~MetgramGraph();


    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(LegendVisitor&);


    // Implements the set method ...
    void set(const map<string, string>& map) { MetgramGraphAttributes::set(map); }
    void set(const XmlNode& node) { MetgramGraphAttributes::set(node); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    double resolution_;


private:
    //! Copy constructor - No copy allowed
    MetgramGraph(const MetgramGraph&);
    //! Overloaded << operator to copy - No copy allowed
    MetgramGraph& operator=(const MetgramGraph&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MetgramGraph& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
