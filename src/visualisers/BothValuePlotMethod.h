/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BothValuePlotMethod.h
    \brief Definition of the Template class BothValuePlotMethod.

    Magics Team - ECMWF 2004

    Started: Thu 26-Aug-2004

    Changes:

*/

#ifndef BothValuePlotMethod_H
#define BothValuePlotMethod_H

#include "magics.h"

#include "BothValuePlotMethodAttributes.h"
#include "Symbol.h"
#include "ValuePlotMethod.h"
#include "ValuePlotMethodAttributes.h"

namespace magics {

class Transformation;


class BothValuePlotMethod : public ValuePlotMethod, public BothValuePlotMethodAttributes {
public:
    BothValuePlotMethod() : marker_(0) {}
    virtual ~BothValuePlotMethod() override {}
    virtual void set(const map<string, string>& map) override {
        BothValuePlotMethodAttributes::set(map);
        ValuePlotMethodAttributes::set(map);
    }
    virtual void set(const XmlNode& node) override {
        BothValuePlotMethodAttributes::set(node);
        ValuePlotMethodAttributes::set(node);
    }
    virtual ValuePlotMethod* clone() const override {
        BothValuePlotMethod* object = new BothValuePlotMethod();
        object->clone(*this);
        return object;
    }

    virtual void clone(const BothValuePlotMethod& from) {
        BothValuePlotMethodAttributes::copy(from);
        ValuePlotMethodAttributes::copy(from);
    }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const override {
        out << "BothValuePlotMethod[";
        BothValuePlotMethodAttributes::print(out);
        ValuePlotMethodAttributes::print(out);
        out << "]";
    }
    void reset() override { marker_ = 0; }

    virtual void add(const PaperPoint& xy) override {
        static map<string, TextPosition> poshandlers;
        if (poshandlers.empty()) {
            poshandlers["none"]   = TextPosition::NONE;
            poshandlers["left"]   = TextPosition::LEFT;
            poshandlers["top"]    = TextPosition::ABOVE;
            poshandlers["bottom"] = TextPosition::BELOW;
            poshandlers["right"]  = TextPosition::RIGHT;
            poshandlers["centre"] = TextPosition::CENTRE;
        }
        if (!marker_) {
            marker_                                 = new TextSymbol();
            map<string, TextPosition>::iterator pos = poshandlers.find(lowerCase(position_));
            TextPosition position                   = (pos != poshandlers.end()) ? pos->second : TextPosition::ABOVE;
            marker_->position(position);
            marker_->setMarker(marker_index_);
            marker_->setColour(*marker_colour_);
            marker_->setHeight(marker_height_);
            marker_->blanking(false);
            MagFont font;
            font.size(this->height_);
            font.colour(*this->colour_);
            marker_->font(font);
            this->push_back(marker_);
        }
        marker_->push_back(xy, this->label(xy.value()));
    }

    TextSymbol* marker_;


private:
    //! Copy constructor - No copy allowed
    BothValuePlotMethod(const BothValuePlotMethod&);
    //! Overloaded << operator to copy - No copy allowed
    BothValuePlotMethod& operator=(const BothValuePlotMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BothValuePlotMethod& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics


#endif
