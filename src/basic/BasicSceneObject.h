/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BasicSceneObject.h
    \brief Definition of the Template class BasicSceneObject.

    Magics Team - ECMWF 2007

    Started: Thu 1-Mar-2007

    Changes:

*/

#ifndef BasicSceneObject_H
#define BasicSceneObject_H

#include "AutoVector.h"
#include "BasicGraphicsObject.h"
#include "DisplayManager.h"
#include "Layout.h"
#include "MagLog.h"
#include "MagicsEvent.h"
#include "magics.h"

//#include "SceneVisitor.h"

namespace magics {

class XmlNode;
class UserPoint;
class UserPoint;

class Transformation;
class AnimationRules;


class DrawingVisitor;
class TopAxisVisitor;
class BottomAxisVisitor;
class LeftAxisVisitor;
class RightAxisVisitor;
class BackgroundVisitor;
class FrameVisitor;
class TextVisitor;
class MetaDataVisitor;
class LegendVisitor;
class PreviewVisitor;
class HistoVisitor;
class MagnifierVisitor;
class SceneVisitor;
class LayoutManager;
class AnimationStep;
class SceneLayer;

class MetaDataCollector;
class MagnifierCollector;
class ValuesCollector;
class DataIndexCollector;
class BinningObject;

class Data;
class Visdef;

class BasicPositionalObject;
class BasicSceneNode;

enum MagicsMode
{
    interactif,
    paper,
    basic
};

class BasicSceneObject : public MetviewIcon {
public:
    BasicSceneObject(BasicSceneObject* parent = 0);
    virtual ~BasicSceneObject() override;

    virtual void push_front(BasicSceneObject* item) {
        item->parent(this);
        items_.insert(items_.begin(), item);
    }
    void push_back(BasicSceneObject* item) {
        item->parent(this);
        items_.push_back(item);
    }

    bool items_empty() { return items_.empty(); }

    virtual void text(TextVisitor* text) {
        ASSERT(parent_);
        parent_->text(text);
    }
    virtual void legend(LegendVisitor* legend) {
        ASSERT(parent_);
        parent_->legend(legend);
    }
    virtual void getReady(const LegendVisitor&);


    virtual void binning(BinningObject*) { MagLog::warning() << "binning(BinningObject*) to be checked" << endl; }
    virtual void data(Data*) { MagLog::warning() << "data(Data*) to be checked" << endl; }
    virtual void visdef(Visdef*) { MagLog::warning() << "visdef(Data*) to be checked" << endl; }
    virtual void set2D() { MagLog::warning() << "visdef(Data*) to be checked" << endl; }

    void addVisitor(SceneVisitor* visitor) { visitors_.push_back(visitor); }

    void parent(BasicSceneObject* parent) { parent_ = parent; }
    void reparent(BasicSceneObject* parent) { parent_ = parent; }

    void orphan() { parent_ = 0; }
    BasicSceneObject& parent() const {
        ASSERT(parent_);
        return *parent_;
    }
    virtual void set(const XmlNode&) {
        MagLog::dev() << "Warning:  BasicSceneObject::set(const XmlNode&)-->Not implemented!" << endl;
    }
    virtual void resolve();
    virtual Layout* execute(AnimationStep&, Layout&) {
        MagLog::dev() << "Warning:  BasicGraphicsObject* BasicSceneObject::execute(AnimsationStep&))-->Not implemented!"
                      << endl;
        return 0;
    }
    virtual void getReady() { dispatch(&BasicSceneObject::getReady); }
    virtual void release() { dispatch(&BasicSceneObject::release); }

    virtual void visit(MetaDataVisitor& meta) { dispatch(meta); }
    virtual void visit(PreviewVisitor& preview) { dispatch(preview); }
    virtual void visit(HistoVisitor& histo) { dispatch(histo); }
    virtual void visit(MagnifierVisitor& magnifier) { dispatch(magnifier); }

    virtual void visit(DateDescription& timestamp) { dispatch(timestamp); }
    virtual void visit(LevelDescription& level) { dispatch(level); }

    virtual void visit(Transformation& transformation) { dispatch(transformation); }
    virtual void visit(AnimationRules& rules) { dispatch(rules); }

    virtual void visit(MetaDataCollector& infos) override { dispatch(infos); }
    virtual void visit(MagnifierCollector& infos) { dispatch(infos); }
    virtual void visit(ValuesCollector& infos) { dispatch(infos); }
    virtual void visit(DataIndexCollector& infos) { dispatch(infos); }

    virtual void visit(DrawingVisitor& drawing) { dispatch(drawing); }
    virtual void visit(TopAxisVisitor& top) { dispatch(top); }
    virtual void visit(BottomAxisVisitor& bottom) { dispatch(bottom); }
    virtual void visit(LeftAxisVisitor& left) { dispatch(left); }
    virtual void visit(RightAxisVisitor& right) { dispatch(right); }
    virtual void visit(BackgroundVisitor& background) { dispatch(background); }
    virtual void visit(FrameVisitor& frame) { dispatch(frame); }

    virtual void visit(TextVisitor& text) { dispatch(text); }
    virtual void visit(LegendVisitor& legend) { dispatch(legend); }
    virtual void visit(SceneLayer& tree) { dispatch(tree); }
    virtual void visit(BasicGraphicsObjectContainer& tree) { dispatch(tree); }
    virtual void visit(AnimationStep& step) { dispatch(step); }
    virtual void visit(SceneLayer& visitor, vector<LayoutVisitor*>& args) { dispatch(visitor, args); }

    virtual bool needLegend();

    virtual void execute() {
        ASSERT(parent_);
        return parent_->execute();
    }

    virtual BasicGraphicsObject* visualise() {
        ASSERT(parent_);
        return parent_->visualise();
    }
    virtual MagicsMode mode() {
        ASSERT(parent_);
        return parent_->mode();
    }

    virtual Transformation& transformation() const {
        ASSERT(parent_);
        return parent_->transformation();
    }
    virtual const Layout& layout() const {
        ASSERT(parent_);
        return parent_->layout();
    }
    virtual BasicGraphicsObject* toDisplay() {
        ASSERT(parent_);
        return parent_->toDisplay();
    }
    virtual double absoluteWidth() const {
        ASSERT(parent_);
        return parent_->absoluteWidth();
    }
    virtual double absoluteHeight() const {
        ASSERT(parent_);
        return parent_->absoluteHeight();
    }
    virtual void absoluteRootWidth(double width) {
        ASSERT(parent_);
        return parent_->absoluteRootWidth(width);
    }
    virtual void absoluteRootHeight(double height) {
        ASSERT(parent_);
        return parent_->absoluteRootHeight(height);
    }
    virtual int rootWidthResolution() const {
        ASSERT(parent_);
        return parent_->rootWidthResolution();
    }
    virtual int rootHeightResolution() const {
        ASSERT(parent_);
        return parent_->rootHeightResolution();
    }
    virtual int widthResolution() const {
        ASSERT(parent_);
        return parent_->rootWidthResolution();
    }
    virtual int heightResolution() const {
        ASSERT(parent_);
        return parent_->rootHeightResolution();
    }

    virtual string theme() const  // REturn the parent theme!
    {
        return (parent_) ? parent_->theme() : "magics";
    }

    const string& name() const { return name_; }
    void name(const string& name) { name_ = name; }

    virtual BasicSceneNode* insert(BasicPositionalObject*) {
        ASSERT(false);
        return 0;
    }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    typedef void (BasicSceneObject::*Function)();


    template <class T>
    void dispatch(T& visitor) {
        for (auto& item : items_)
            item->visit(visitor);
    }
    template <class T1, class T2>
    void dispatch(T1& visitor, T2& args) {
        for (auto& item : items_)
            item->visit(visitor, args);
    }
    void dispatch(Function function) {
        for (auto& item : items_)
            ((*item).*function)();
    }


    AutoVector<BasicSceneObject> items_;
    vector<SceneVisitor*> visitors_;

    BasicSceneObject* parent_;  // Do not delete! Only a reference

    string name_;

private:
    BasicSceneObject(const BasicSceneObject&);
    //! Overloaded << operator to copy - No copy allowed
    BasicSceneObject& operator=(const BasicSceneObject&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const BasicSceneObject& p) {
        p.print(s);
        return s;
    }
};

class BasicPositionalObject : public BasicSceneObject {
public:
    BasicPositionalObject() {}
    virtual ~BasicPositionalObject() override {}
    virtual Layout& layout() const override = 0;

protected:
    double adjustDimension(double, double, double);
};

class BasicSceneNode : public BasicPositionalObject {
public:
    BasicSceneNode();
    BasicSceneNode(Layout*);
    virtual ~BasicSceneNode() override;

    virtual BasicSceneNode* insert(
        BasicPositionalObject*) override;  // Return the node tinto which the object has been inserted!
    Layout& layout() const override {
        {
            ASSERT(layout_);
            return *layout_;
        }
    }
    virtual void getReady() override;
    virtual BasicSceneNode* clone();
    virtual BasicSceneNode* newNode(BasicPositionalObject*);

    virtual void visit(BasicGraphicsObjectContainer& tree) override {
        tree.push_back(layout_);
        layout_->blankIt();
        dispatch(*layout_);
        layout_->frameIt();
    }
    void newpage();
    double absoluteWidth() const override;
    double absoluteHeight() const override;


    void manager(LayoutManager* manager) { manager_ = manager; }

protected:
    Layout* layout_;
    LayoutManager* manager_;
    friend class LayoutManager;
};

class EmptySceneObject : public BasicSceneObject {
public:
    EmptySceneObject();
    virtual ~EmptySceneObject() override;


    void visit(SceneLayer&, vector<LayoutVisitor*>&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;


private:
    //! Copy constructor - No copy allowed
    EmptySceneObject(const EmptySceneObject&);
    //! Overloaded << operator to copy - No copy allowed
    EmptySceneObject& operator=(const EmptySceneObject&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const EmptySceneObject&) {
        s << "EmptySceneObject";
        return s;
    }
};

class FrameBackgroundObject : public BasicSceneObject {
public:
    FrameBackgroundObject(bool blankIt, const Colour& colour) : blankIt_(blankIt), colour_(colour) {}
    virtual ~FrameBackgroundObject() override{};

    void visit(DrawingVisitor&) override;
    void visit(SceneLayer&, vector<LayoutVisitor*>&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).

    bool blankIt_;
    Colour colour_;


private:
    //! Copy constructor - No copy allowed
    FrameBackgroundObject(const FrameBackgroundObject&);
    //! Overloaded << operator to copy - No copy allowed
    FrameBackgroundObject& operator=(const FrameBackgroundObject&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const FrameBackgroundObject&) {
        s << "FrameBackgroundObject";
        return s;
    }
};

class FrameForegroundObject : public BasicSceneObject {
public:
    FrameForegroundObject(bool frameIt, const Colour& colour, LineStyle style, int thickness) :
        frameIt_(frameIt), colour_(colour), style_(style), thickness_(thickness) {}

    virtual ~FrameForegroundObject() override{};

    void visit(DrawingVisitor&) override;
    void visit(SceneLayer&, vector<LayoutVisitor*>&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).

    bool frameIt_;
    Colour colour_;
    LineStyle style_;
    int thickness_;

private:
    //! Copy constructor - No copy allowed
    FrameForegroundObject(const FrameForegroundObject&);
    //! Overloaded << operator to copy - No copy allowed
    FrameForegroundObject& operator=(const FrameForegroundObject&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const FrameForegroundObject&) {
        s << "FrameForegroundObject";
        return s;
    }
};

}  // namespace magics
#endif
