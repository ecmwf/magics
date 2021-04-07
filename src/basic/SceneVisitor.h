/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file SceneVisitor.h
    \brief Definition of the Template class SceneVisitor.

    Magics Team - ECMWF 2008

    Started: Mon 29-Dec-2008

    Changes:

*/

#ifndef SceneVisitor_H
#define SceneVisitor_H

#include "BasicGraphicsObject.h"
#include "BasicSceneObject.h"
#include "magics.h"

namespace magics {
class Layout;

class SceneVisitor {
public:
    SceneVisitor();
    virtual ~SceneVisitor();
    bool reproject(BasicGraphicsObjectContainer&) const;
    virtual void visit(BasicSceneObject& object) = 0;
    virtual void visit(BasicGraphicsObjectContainer&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream
    //! (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    SceneVisitor(const SceneVisitor&);
    //! Overloaded << operator to copy - No copy allowed
    SceneVisitor& operator=(const SceneVisitor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SceneVisitor& p) {
        p.print(s);
        return s;
    }
};

class LayoutVisitor : public SceneVisitor {
public:
    LayoutVisitor() : layout_(0), current_(0) {}
    ~LayoutVisitor() override {
        if (layout_)
            delete layout_;
    }
    void redisplay(const BaseDriver& driver) const;

    virtual void reset() {}
    virtual void set(MagnifierCollector&) {}
    virtual void newLayout() const;

    Layout& layout() const;
    Layout* layoutPtr() const;
    Layout* mainLayout() const;

    const Transformation& transformation() const {
        ASSERT(layout_);
        return layout_->transformation();
    }
    void push_back(BasicGraphicsObject* object);

    void parent(BasicGraphicsObjectContainer* parent) {
        ASSERT(layout_);
        layout_->parent(parent);
    }
    double minX() const {
        ASSERT(layout_);
        return layout_->minX();
    }
    double maxX() const {
        ASSERT(layout_);
        return layout_->maxX();
    }
    double minY() const {
        ASSERT(layout_);
        return layout_->minY();
    }
    double maxY() const {
        ASSERT(layout_);
        return layout_->maxY();
    }
    void width(double width) {
        ASSERT(layout_);
        return layout_->width(width);
    }
    void height(double height) {
        ASSERT(layout_);
        return layout_->height(height);
    }
    void widthResolution(double width) {
        ASSERT(layout_);
        return layout_->widthResolution(width);
    }
    void heightResolution(double height) {
        ASSERT(layout_);
        return layout_->heightResolution(height);
    }
    void x(double x) {
        ASSERT(layout_);
        return layout_->x(x);
    }
    void y(double y) {
        ASSERT(layout_);
        return layout_->y(y);
    }
    void transformation(Layout& layout) const { layout.transformation(layout_->transformation_); }
    void transformation(Transformation* transformation) {
        ASSERT(layout_);
        return layout_->transformation(transformation);
    }
    void id(const string& id) {
        ASSERT(layout_);
        return layout_->id(id);
    }
    void zoomable(bool zoomable) {
        ASSERT(layout_);
        return layout_->zoomable(zoomable);
    }
    void zoomLevels(int zl) {
        ASSERT(layout_);
        return layout_->zoomLevels(zl);
    }
    void zoomCurrentLevel(int level) {
        ASSERT(layout_);
        return layout_->zoomCurrentLevel(level);
    }
    void frameIt() {
        ASSERT(layout_);
        return layout_->frameIt();
    }
    void blankIt(const string& colour = "white") {
        ASSERT(layout_);
        return layout_->blankIt(colour);
    }
    void frame(Layout& layout) {
        ASSERT(layout_);
        layout_->frame(layout);
    }
    void clippIt(bool clip) {
        ASSERT(layout_);
        layout_->clippIt(clip);
    }

protected:
    mutable Layout* layout_;
    mutable Layout* current_;

    virtual void print(std::ostream& s) const override;
};

class DrawingVisitor : public LayoutVisitor {
public:
    DrawingVisitor();
    ~DrawingVisitor() override;
    void visit(BasicSceneObject& object) override { object.visit(*this); }
    void set(MagnifierCollector&) override;
    // Layout* execute(AnimationStep&, const Layout* visitor);

protected:
    virtual void print(std::ostream& s) const override { s << "DrawingVisitor[]"; };
};
class FrameVisitor : public LayoutVisitor {
public:
    FrameVisitor();
    ~FrameVisitor() override;

    void backgroundColour(const string& colour) { background_ = colour; }

    void visit(BasicSceneObject& object) override { object.visit(*this); }
    void frameIt() {
        if (current_)
            current_->frameIt();
    }
    void blankIt() {
        if (current_)
            current_->blankIt(background_);
    }

protected:
    string background_;
    virtual void print(std::ostream& s) const override { s << "FrameVisitor[]"; };
};

class HorizontalAxisVisitor : public LayoutVisitor {
public:
    HorizontalAxisVisitor(const DrawingVisitor&);
    ~HorizontalAxisVisitor() override;
    virtual void tick(double&, double&, bool);
    virtual void minortick(double&, double&, bool);
    virtual double offsetTickLabel(double, double);
    virtual double angleTickLabel();
    virtual double angleTitle();
    virtual double offsetTitle(int);
    virtual double angleTip();
    virtual double offsetTip();
    virtual VerticalAlign textAlignment(const string&) { return VerticalAlign::BOTTOM; }
    virtual Justification justificationTickLabel(const string&) { return Justification::CENTRE; }
    virtual double percentX(double) { return 0; }
    virtual double percentY(double) { return 0; }


protected:
    virtual void print(std::ostream& s) const override { s << "HorizontalAxisVisitor[]"; };
};
class VerticalAxisVisitor : public LayoutVisitor {
public:
    VerticalAxisVisitor(const DrawingVisitor&);
    ~VerticalAxisVisitor() override;
    virtual double offsetLine() { return 10; }
    virtual void tick(double&, double&, bool);
    virtual void minortick(double&, double&, bool);
    virtual double offsetTickLabel(double, double);
    virtual double angleTickLabel();
    virtual double angleTitle();
    virtual double offsetTitle(int);
    virtual double shiftTitle(double x) { return x; }
    virtual double angleTip();
    virtual double offsetTip();
    virtual Justification justificationTickLabel(const string&);
    virtual VerticalAlign textAlignment(const string&) { return VerticalAlign::HALF; }
    virtual double percentX(double) { return 0; }
    virtual double percentY(double) { return 0; }

protected:
    virtual void print(std::ostream& s) const override { s << "VerticalAxisVisitor[]"; };
};
class LeftAxisVisitor : public VerticalAxisVisitor {
public:
    LeftAxisVisitor(const DrawingVisitor&);
    ~LeftAxisVisitor() override;
    void visit(BasicSceneObject& object) override { object.visit(*this); }
    virtual Justification justificationTickLabel(const string&) override;
    virtual void tick(double&, double&, bool) override;
    virtual void minortick(double&, double&, bool) override;
    virtual double offsetTickLabel(double, double) override;
    virtual double angleTickLabel() override;
    virtual double angleTitle() override;
    virtual double shiftTitle(double x) override;
    virtual double offsetTitle(int) override;
    virtual double angleTip() override;
    virtual double offsetTip() override;
    virtual VerticalAlign textAlignment(const string&) override;
    virtual double percentX(double) override;
    virtual double percentY(double) override;

protected:
    virtual void print(std::ostream& s) const override { s << "LeftAxisVisitor[]"; };
};

class RightAxisVisitor : public VerticalAxisVisitor {
public:
    RightAxisVisitor(const DrawingVisitor&);
    ~RightAxisVisitor() override;
    void visit(BasicSceneObject& object) override { object.visit(*this); }
    virtual Justification justificationTickLabel(const string&) override;
    virtual void tick(double&, double&, bool) override;
    virtual void minortick(double&, double&, bool) override;
    virtual double offsetTickLabel(double, double) override;
    virtual double angleTickLabel() override;
    virtual double angleTitle() override;
    virtual double offsetTitle(int) override;
    virtual double shiftTitle(double) override;
    virtual double angleTip() override;
    virtual double offsetTip() override;
    virtual VerticalAlign textAlignment(const string&) override;
    virtual double percentX(double) override;
    virtual double percentY(double) override;

protected:
    virtual void print(std::ostream& s) const override { s << "RightAxisVisitor[]"; };
};

class TopAxisVisitor : public HorizontalAxisVisitor {
public:
    TopAxisVisitor(const DrawingVisitor&);
    ~TopAxisVisitor() override;
    void visit(BasicSceneObject& object) override { object.visit(*this); }
    virtual Justification justificationTickLabel(const string&) override;
    virtual void tick(double&, double&, bool) override;
    virtual void minortick(double&, double&, bool) override;
    virtual double offsetTickLabel(double, double) override;
    virtual double angleTickLabel() override;
    virtual double angleTitle() override;
    virtual double offsetTitle(int) override;
    virtual double angleTip() override;
    virtual double offsetTip() override;
    virtual VerticalAlign textAlignment(const string&) override;
    virtual double percentX(double) override;
    virtual double percentY(double) override;

protected:
    virtual void print(std::ostream& s) const override { s << "TopAxisVisitor[]"; };
};

class BottomAxisVisitor : public HorizontalAxisVisitor {
public:
    BottomAxisVisitor(const DrawingVisitor&);
    ~BottomAxisVisitor() override;
    void visit(BasicSceneObject& object) override { object.visit(*this); }
    virtual void tick(double&, double&, bool) override;
    virtual void minortick(double&, double&, bool) override;
    virtual Justification justificationTickLabel(const string&) override;
    virtual double offsetTickLabel(double, double) override;
    virtual double angleTickLabel() override;
    virtual double angleTitle() override;
    virtual double offsetTitle(int) override;
    virtual double angleTip() override;
    virtual double offsetTip() override;
    virtual VerticalAlign textAlignment(const string&) override;
    virtual double percentX(double) override;
    virtual double percentY(double) override;

protected:
    virtual void print(std::ostream& s) const override { s << "BottomAxisVisitor[]"; };
};

}  // namespace magics
#endif
