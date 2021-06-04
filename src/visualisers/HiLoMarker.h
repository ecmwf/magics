/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HiLoMarker.h
    \brief Definition of the Template class HiLoMarker.

    Magics Team - ECMWF 2004

    Started: Tue 22-Jun-2004

    Changes:

*/

#ifndef HiLoMarker_H
#define HiLoMarker_H

#include "HiLo.h"
#include "HiLoMarkerAttributes.h"
#include "HiLoMarkerBase.h"
#include "MatrixHandler.h"
#include "PointsHandler.h"
#include "Symbol.h"
#include "magics.h"
namespace magics {


class HiLoMarker : public HiLoMarkerBase, public HiLoMarkerAttributes {
public:
    HiLoMarker() : marker_(0) {}
    virtual ~HiLoMarker() override {}
    void set(const map<string, string>& map) override { HiLoMarkerAttributes::set(map); }
    void set(const XmlNode& node) override { HiLoMarkerAttributes::set(node); }
    virtual bool accept(const string& node) override { return HiLoMarkerAttributes::accept(node); }

    virtual HiLoMarker* clone() const override {
        HiLoMarker* object = new HiLoMarker();
        object->copy(*this);
        return object;
    }
    virtual void clear() override { marker_ = 0; }

    virtual void operator()(const PaperPoint& point, HiLo& hilo) override {
        if (!marker_) {
            marker_ = new Symbol();
            marker_->setMarker(index_);
            marker_->setColour(*colour_);
            marker_->setHeight(height_);
            hilo.push_back(marker_);
        }
        marker_->push_back(point);
    }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override {
        out << "HiLoMarker[";
        HiLoMarkerAttributes::print(out);
        out << "]";
    }
    double scaling_;
    Symbol* marker_;


private:
    //! Copy constructor - No copy allowed
    HiLoMarker(const HiLoMarker&);
    //! Overloaded << operator to copy - No copy allowed
    HiLoMarker& operator=(const HiLoMarker&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const HiLoMarker& p) {
        p.print(s);
        return s;
    }
};


class NoHiLoMarker : public HiLoMarkerBase {
public:
    NoHiLoMarker() {}
    virtual ~NoHiLoMarker() override {}
    virtual HiLoMarkerBase* clone() const override {
        HiLoMarkerBase* object = new NoHiLoMarker();
        return object;
    }
    virtual bool accept(const string& node) override { return magCompare(node, "nohilo"); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override { out << "No HiLo Marker defined"; }
};


}  // namespace magics
#endif
