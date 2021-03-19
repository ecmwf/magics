/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Axis.h
    \brief Definition of the Template class Axis.

    Magics Team - ECMWF 2004

    Started: Fri 7-May-2004

    Changes:

*/

#ifndef Axis_H
#define Axis_H

#include "magics.h"

#include "AxisAttributes.h"
#include "BasicSceneObject.h"
#include "MagicsEvent.h"
#include "XmlNode.h"
namespace magics {

class XmlNode;

class SceneLayer;
class LayoutVisitor;
class AxisItem;

class Axis : public BasicSceneObject, public AxisAttributes {
public:
    Axis();
    virtual ~Axis() override;

    void set(const XmlNode& node) override { AxisAttributes::set(node); }
    void set(const map<string, string>& map) override { AxisAttributes::set(map); }

    void visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors) override;
    void visit(TextVisitor&) override;

    virtual void tick(VerticalAxisVisitor&) {}
    virtual void tick(HorizontalAxisVisitor&) {}

    virtual void minortick(VerticalAxisVisitor&) {}
    virtual void minortick(HorizontalAxisVisitor&) {}

    virtual void label(VerticalAxisVisitor&) {}
    virtual void label(HorizontalAxisVisitor&) {}

    virtual void title(VerticalAxisVisitor&) {}
    virtual void title(HorizontalAxisVisitor&) {}

    virtual void line(TopAxisVisitor& out) const {}
    virtual void line(BottomAxisVisitor& out) const {}
    virtual void line(LeftAxisVisitor& out) const {}
    virtual void line(RightAxisVisitor& out) const {}

    virtual void tip(TopAxisVisitor& out) const {}
    virtual void tip(BottomAxisVisitor& out) const {}
    virtual void tip(LeftAxisVisitor& out) const {}
    virtual void tip(RightAxisVisitor& out) const {}

    virtual void grid(DrawingVisitor&) const {}

    string createLabel(const AxisItem&);

    double min() const { return method_->min(); }
    double max() const { return method_->max(); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    void ticks(double, double, vector<double>&);
    AxisItems items_;


    typedef string (Axis::*LabelHelper)(const AxisItem&);
    map<string, LabelHelper> labelHelpers_;

    string number(const AxisItem&);
    string labellist(const AxisItem&);
    string latitude(const AxisItem&);
    string longitude(const AxisItem&);
    string basic(const AxisItem&);
    double title_position_;

    int currentLabel_;

private:
    //! Copy constructor - No copy allowed
    Axis(const Axis&);
    //! Overloaded << operator to copy - No copy allowed
    Axis& operator=(const Axis&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Axis& p) {
        p.print(s);
        return s;
    }
};


class HorizontalAxis : public Axis {
public:
    HorizontalAxis();
    ~HorizontalAxis() override {}

    template <class V>
    void build(V& visitor) {
        if (items_.empty()) {
            method_->updateX(visitor.transformation());
            method_->prepare(*this, items_);
        }
        line(visitor);
        tick(visitor);
        label(visitor);
        title(visitor);
        minortick(visitor);
        tip(visitor);
    }

    void visit(DrawingVisitor& visitor) override {
        if (items_.empty()) {
            method_->updateX(visitor.transformation());
            method_->prepare(*this, items_);
        }
        grid(visitor);
    }

    void visit(TopAxisVisitor& visitor) override {
        if (magCompare(position_, "top"))
            build(visitor);
    }
    void visit(BottomAxisVisitor& visitor) override {
        if (magCompare(position_, "bottom"))
            build(visitor);
    }

    void set(const XmlNode& node) override {
        if (magCompare(node.name(), "horizontal_axis")) {
            XmlNode axis = node;
            axis.name("axis");
            AxisAttributes::set(axis);
        }
    }

    void minortick(HorizontalAxisVisitor&) override;
    void tick(HorizontalAxisVisitor&) override;
    void label(HorizontalAxisVisitor&) override;
    void title(HorizontalAxisVisitor&) override;

    void line(TopAxisVisitor& out) const override;
    void line(BottomAxisVisitor& out) const override;

    void tip(TopAxisVisitor& out) const override;
    void tip(BottomAxisVisitor& out) const override;

    void grid(DrawingVisitor&) const override;
};

class VerticalAxis : public Axis {
public:
    VerticalAxis();
    ~VerticalAxis() override {}
    template <class V>
    void build(V& visitor) {
        if (items_.empty()) {
            method_->updateY(visitor.transformation());
            method_->prepare(*this, items_);
        }
        line(visitor);
        tick(visitor);
        label(visitor);
        title(visitor);
        minortick(visitor);
        tip(visitor);
    }
    void visit(DrawingVisitor& visitor) override {
        if (items_.empty()) {
            method_->updateY(visitor.transformation());
            method_->prepare(*this, items_);
        }
        grid(visitor);
    }
    void visit(LeftAxisVisitor& visitor) override {
        if (magCompare(position_, "left"))
            build(visitor);
    }
    void visit(RightAxisVisitor& visitor) override {
        if (magCompare(position_, "right"))
            build(visitor);
    }
    void set(const XmlNode& node) override {
        if (magCompare(node.name(), "vertical_axis")) {
            XmlNode axis = node;
            axis.name("axis");
            AxisAttributes::set(axis);
        }
    }
    void tick(VerticalAxisVisitor&) override;
    void minortick(VerticalAxisVisitor&) override;
    void label(VerticalAxisVisitor&) override;
    void title(VerticalAxisVisitor&) override;

    void line(LeftAxisVisitor& out) const override;
    void line(RightAxisVisitor& out) const override;

    void tip(LeftAxisVisitor& out) const override;
    void tip(RightAxisVisitor& out) const override;

    void grid(DrawingVisitor&) const override;
};

}  // namespace magics
#endif
