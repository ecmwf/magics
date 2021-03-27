/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Wind.h
    \brief Definition of the Template class Wind.

    Magics Team - ECMWF 2005

    Started: Thu 17-Mar-2005

    Changes:

*/

#ifndef Wind_H
#define Wind_H

#include "magics.h"

#include "ThinningMethod.h"
#include "Visdef.h"
#include "WindAttributes.h"

namespace magics {


class Wind : public Visdef, public WindAttributes, public ThinningMethodUI {
public:
    Wind();
    virtual ~Wind() override;

    virtual void set(const map<string, string>& map) override { WindAttributes::set(map); }
    virtual void set(const XmlNode& node) override { WindAttributes::set(node); }

    // implements the Visdef interface ...
    void operator()(Data&, BasicGraphicsObjectContainer&) override;
    void visit(LegendVisitor&) override;
    bool needLegend() override { return type_->needLegend(); }
    void getReady(const LegendVisitor& legend) override {
        Visdef::getReady(legend);
        type_->legendOnly(legendOnly_);
    }
    void visit(Data& data, HistoVisitor& visitor) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;


private:
    //! Copy constructor - No copy allowed
    Wind(const Wind&);
    //! Overloaded << operator to copy - No copy allowed
    Wind& operator=(const Wind&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Wind& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics


#endif
