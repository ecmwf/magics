/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HiLo.h
    \brief Definition of the Template class HiLo.

    Magics Team - ECMWF 2004

    Started: Tue 22-Jun-2004

    Changes:

*/

/*! \defgroup hilo Computation of High and Lows

   \section hiloOverview Overview

   Magics++ supports the calculation of Highs and Lows.
   These can be plotted (as Letters, Values or Symbols)
   or written into a file.

   In the past (MAGICS 6 and Magics++ <2.8.1) simple tests
   were used to find local mins and maxs. This resulted in
   comments that Magics did too many (not meaningful) extremas.

   \sa MinMaxFilter::Process()
*/

#ifndef HiLo_H
#define HiLo_H

#include "BasicSceneObject.h"
#include "HiLoAttributes.h"
#include "HiLoBase.h"
#include "magics.h"

namespace magics {

class MatrixHandler;

class HiLo : public HiLoBase, public HiLoAttributes, public vector<BasicGraphicsObject*> {
public:
    HiLo();
    virtual ~HiLo() override;
    virtual void set(const map<string, string>&) override;
    virtual void set(const XmlNode& node) override { HiLoAttributes::set(node); }
    virtual bool accept(const string& node) override {
        return HiLoAttributes::accept(node);
        ;
    }

    virtual HiLoBase* clone() const override {
        HiLo* hilo = new HiLo();
        hilo->copy(*this);
        return hilo;
    }

    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) override;
    virtual void operator()(const PaperPoint&);
    virtual void clear() override {
        this->type_->clear();
        this->marker_->clear();
        vector<BasicGraphicsObject*>::clear();
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    HiLo(const HiLo&);
    //! Overloaded << operator to copy - No copy allowed
    HiLo& operator=(const HiLo&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const HiLo& p) {
        p.print(s);
        return s;
    }
};
class NoHiLo : public HiLoBase {
public:
    NoHiLo() {}
    virtual ~NoHiLo() override {}
    HiLoBase* clone() const override { return new NoHiLo(); }
    virtual void set(const map<string, string>&) override {}
    virtual BasicGraphicsObject* next() override { return 0; }
    virtual bool more() override { return false; }
    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) override {}
    void print(ostream& out) const override { out << "NoHiLo[]"; }
};


class HighHiLo : public HiLo {
public:
    HighHiLo() {}
    virtual ~HighHiLo() override {}
    HiLoBase* clone() const override {
        HiLo* hilo = new HighHiLo();
        // hilo->copy(*this);
        return hilo;
    }

    void operator()(const PaperPoint& point) override {
        if (point.high())
            HiLo::operator()(point);
    }
};


class LowHiLo : public HiLo {
public:
    LowHiLo() {}
    virtual ~LowHiLo() override {}
    HiLoBase* clone() const override {
        HiLoBase* hilo = new LowHiLo();
        // hilo->copy(*this);
        return hilo;
    }

    void operator()(const PaperPoint& point) override {
        if (point.low())
            HiLo::operator()(point);
    }
};


}  // namespace magics

#endif
