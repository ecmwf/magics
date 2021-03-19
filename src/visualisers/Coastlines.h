/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Coastlines.h
    \brief Definition of the Template class Coastlines.

    Magics Team - ECMWF 2004

    Started: Thu 29-Jan-2004

    Changes:

*/

#ifndef Coastlines_H
#define Coastlines_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "CoastlinesAttributes.h"
#include "MagicsEvent.h"
#include "SceneVisitor.h"

namespace magics {

class ProgressObject;
class StaticLayer;

class Coastlines : public CoastlinesAttributes, public BasicSceneObject {
public:
    Coastlines();
    virtual ~Coastlines() override;


    // New Interface!
    void visit(DrawingVisitor& list) override;
    void visit(TextVisitor&) override;
    void visit(LegendVisitor&) override;
    void visit(LeftAxisVisitor& list) override;
    void visit(BottomAxisVisitor& list) override;
    void visit(TopAxisVisitor& list) override;
    void visit(RightAxisVisitor& list) override;
    void visit(MetaDataCollector& list) override;

    void visit(PreviewVisitor& list) override;
    void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors) override;
    void visit(Transformation& transformation) override;
    void set(const map<string, string>& map) override { CoastlinesAttributes::set(map); }
    void set(const XmlNode& node) override { CoastlinesAttributes::set(node); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    StaticLayer* layer_;

private:
    //! Copy constructor - No copy allowed
    Coastlines(const Coastlines&);
    //! Overloaded << operator to copy - No copy allowed
    Coastlines& operator=(const Coastlines&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Coastlines& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
