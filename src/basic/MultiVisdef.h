/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MultiVisdef.h
    \brief Definition of the Template class MultiVisdef.

    Magics Team - ECMWF 2012

    Started: Wed 25-Jan-2012

    Changes:

*/

#ifndef MultiVisdef_H
#define MultiVisdef_H

#include "magics.h"

#include "Visdef.h"

namespace magics {

class MultiVisdef : public Visdef {
public:
    MultiVisdef();
    virtual ~MultiVisdef() override;
    vector<Visdef*>* oneDimension() { return &one_d_; }
    vector<Visdef*>* twoDimension() { return &two_d_; }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(LegendVisitor&) override;
    virtual void visit(Data&, LegendVisitor& legend) override;
    virtual void visit(Data&, HistoVisitor&) override;
    virtual void visit(MetaDataVisitor&) override;
    virtual void visit(TopAxisVisitor&) override;
    virtual void visit(Transformation&, Data&) override;
    virtual void visit(Layer& layer) override;
    virtual void beanInfo(IntervalMap<Colour>&) override;
    bool needLegend() override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    vector<Visdef*> one_d_;
    vector<Visdef*> two_d_;


private:
    //! Copy constructor - No copy allowed
    MultiVisdef(const MultiVisdef&);
    //! Overloaded << operator to copy - No copy allowed
    MultiVisdef& operator=(const MultiVisdef&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MultiVisdef& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
