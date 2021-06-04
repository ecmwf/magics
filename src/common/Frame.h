/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Frame.h
    \brief Definition of the Template class Frame.

    Magics Team - ECMWF 2004

    Started: Mon 29-Mar-2004

    Changes:

*/

#ifndef Frame_H
#define Frame_H

#include "FrameAttributes.h"
#include "FrameBase.h"
#include "magics.h"

namespace magics {

class PaperPoint;
class Polyline;


class Frame : public FrameBase, public FrameAttributes {
public:
    Frame();
    virtual ~Frame() override;
    virtual FrameBase* clone() const override;

    void set(const map<string, string>& map) { FrameAttributes::set(map); }
    void set(const XmlNode& xml) { FrameAttributes::set(xml); }

    virtual bool operator()() const override { return true; }
    void set(Polyline&);
    void blank(Polyline&);
    // Simulate the FrameAttributes interface!
    virtual void setColour(Colour* colour) override { FrameAttributes::setColour(colour); }
    virtual void setStyle(LineStyle style) override { FrameAttributes::setStyle(style); }
    virtual void setThickness(int thickness) override { FrameAttributes::setThickness(thickness); }
    virtual void setBlanking(bool blanking) override { FrameAttributes::setBlanking(blanking); }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Frame& p) {
        p.print(s);
        return s;
    }
};
class NoFrame : public FrameBase {
public:
    NoFrame() {}
    ~NoFrame() override {}

    bool operator()() const { return false; }
    FrameBase* clone() const override { return new NoFrame(); }

protected:
    virtual void print(ostream&) const override;
};


}  // namespace magics
#endif
