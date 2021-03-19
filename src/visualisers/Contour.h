/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
#include "MagConfig.h"
#include "Visdef.h"

namespace magics {


class StyleEntry;

class Contour : public ContourAttributes, public Visdef {
public:
    Contour();
    virtual ~Contour() override;

    virtual Contour* clone() const {
        Contour* contour = new Contour();
        contour->copy(*this);
        return contour;
    }
    bool needLegend() override { return legend_; }
    // Implements the set method ...
    void set(const XmlNode& node) override { ContourAttributes::set(node); }
    void set(const map<string, string>& map) override { ContourAttributes::set(map); }

    // Implements the VisDefinterface
    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(Data&, LegendVisitor&) override;

    void visit(Data&, HistoVisitor&) override;
    void visit(MetaDataVisitor&) override;
    void getReady(const LegendVisitor& legend) override {
        contour_->legend_only_ = legend.only_;
        legendIsOn_            = true;
    }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    MatrixHandler* matrix_;
    StyleEntry* styleInfo_;
    MagDef automaticAttributes_;
    bool legendIsOn_;

private:
    //! Copy constructor - No copy allowed
    Contour(const Contour&);
    //! Overloaded << operator to copy - No copy allowed
    Contour& operator=(const Contour&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Contour& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics

#endif
