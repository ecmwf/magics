/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SymbolPlotting.h
    \brief Definition of the Template class SymbolPlotting.

    Magics Team - ECMWF 2004

    Started: Mon 19-Jan-2004

    Changes:

*/

#ifndef SymbolPlotting_H
#define SymbolPlotting_H

#include "magics.h"

#include "PointsHandler.h"
#include "Symbol.h"
#include "SymbolPlottingAttributes.h"
#include "Visdef.h"

namespace magics {

class ProgressObject;


class SymbolPlotting : public SymbolPlottingAttributes, public Visdef {
public:
    SymbolPlotting();
    virtual ~SymbolPlotting() override;


    // Implements the Visualiser Interface...


    virtual void operator()(Data&, BasicGraphicsObjectContainer&) override;
    virtual void visit(Data&, LegendVisitor&) override;
    bool needLegend() override { return legend_; }
    virtual void visit(Data&, HistoVisitor&) override;
    void operator()(const PaperPoint&, BasicGraphicsObjectContainer&) const;
    void getReady(const LegendVisitor& legend) override;

    void set(const map<string, string>& map) override { SymbolPlottingAttributes::set(map); }
    void set(const XmlNode& node) override { SymbolPlottingAttributes::set(node); }
    double height(const Transformation&, double height);  // compute symbol_size!
protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

    mutable map<SymbolProperties, Symbol*> symbols_;
    map<SymbolProperties, Symbol*>::iterator current_;
    mutable vector<Text*> texts_;
    vector<string>::iterator text_;


private:
    //! Copy constructor - No copy allowed
    SymbolPlotting(const SymbolPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    SymbolPlotting& operator=(const SymbolPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SymbolPlotting& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
