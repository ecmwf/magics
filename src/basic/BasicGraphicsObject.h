/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BasicGraphicsObject.h
    \brief Implementation of BasicGraphicsObject class.
    \author Meteorological Visualisation Section, ECMWF

    Started: March 2004

*/
#ifndef BasicGraphicsObject_H
#define BasicGraphicsObject_H

#include "magics.h"

#include <algorithm>

#include "AutoVector.h"
#include "MagException.h"
#include "MagLog.h"

namespace magics {

class BaseDriver;

class Layout;
class Transformation;

class BasicGraphicsObjectContainer;

class BasicGraphicsObject {
public:
    BasicGraphicsObject();
    virtual ~BasicGraphicsObject();

    virtual bool reproject(BasicGraphicsObjectContainer&) const {
        MagLog::error() << "BasicGraphicsObject::reproject(...)--->Need to be implemented!\n";
        ASSERT(0);
        return false;
    }

    virtual void redisplay(const BaseDriver&) const {
        MagLog::dev() << "BasicGraphicsObject::redisplay(...)--->Not yet implemented\n";
    }

    void parent(BasicGraphicsObjectContainer* parent) {
        // ASSERT(parent_ == 0);
        parent_ = parent;
    }
    void check();
    BasicGraphicsObjectContainer& parent() {
        ASSERT(parent_);
        return *parent_;
    }

    void makeBrother(const BasicGraphicsObject& brother) { parent_ = brother.parent_; }

    virtual void release() {}

    bool isOrphan() { return parent_ == 0; }
    void orphan() { parent_ = 0; }

    void name(const string& name) { name_ = name; }
    const string& name() const { return name_; }
    bool root() { return parent_ == 0; }

    void widthResolution(int width) { widthResolution_ = width; }
    void heightResolution(int height) { heightResolution_ = height; }
    int widthResolution() const { return widthResolution_; }
    int heightResolution() const { return heightResolution_; }

    virtual bool buildTree(const Layout&, unsigned int, const BaseDriver&) const { return false; }

protected:
    virtual void print(ostream&) const;
    int widthResolution_;
    int heightResolution_;

    BasicGraphicsObjectContainer* parent_;  // Just for reference : do not delete!
    string name_;

private:
    // No copy allowed
    // BasicGraphicsObject& operator=(const BasicGraphicsObject&);

    // -- Friends
    friend ostream& operator<<(ostream& s, const BasicGraphicsObject& p) {
        p.print(s);
        return s;
    }
};

/*!

 Inherited by SceneLayer

 \sa SceneLayer
*/
class BasicGraphicsObjectContainer : public BasicGraphicsObject {
public:
    BasicGraphicsObjectContainer() {}
    virtual ~BasicGraphicsObjectContainer();

    void push_back(BasicGraphicsObject* object) {
        object->check();  // here we make sure that the object is not in 2 containres!
        objects_.push_back(object);
        object->parent(this);
    }
    void push_last(BasicGraphicsObject* object) {
        object->check();  // here we make sure that the object is not in 2 containres!
        last_.push_back(object);
        object->parent(this);
    }

    void clear();
    bool buildTree(const Layout&, unsigned int, const BaseDriver&) const;
    void release();
    void remove(BasicGraphicsObject* object) {
        objects_.erase(std::remove(objects_.begin(), objects_.end(), object), objects_.end());
    }

    void visit(const BaseDriver&) const;

    virtual void getDriverInfo(double& x, double& y, double& width, double& height) {
        if (parent_)
            parent_->getDriverInfo(x, y, width, height);
    }

    virtual double absoluteY() const  // absolute position from the root
    {
        ASSERT(parent_);
        return parent_->absoluteY();
    }

    virtual double absoluteWidth() const  // absolute position from the root
    {
        ASSERT(parent_);
        return parent_->absoluteWidth();
    }

    virtual double absoluteHeight() const  // absolute position from the root
    {
        ASSERT(parent_);
        return parent_->absoluteHeight();
    }

    virtual double absoluteWidth(double width)  // absolute position from the root
    {
        ASSERT(parent_);
        return parent_->absoluteWidth(width);
    }

    virtual double absoluteHeight(double height)  // absolute position from the root
    {
        ASSERT(parent_);
        return parent_->absoluteHeight(height);
    }

    virtual const Transformation& transformation() const  // returns the Transformation
    {
        ASSERT(parent_);
        return parent_->transformation();
    }
    const vector<BasicGraphicsObject*>& objects() {  //
        // first we add
        for (vector<BasicGraphicsObject*>::iterator l = last_.begin(); l != last_.end(); ++l)
            objects_.push_back(*l);
        last_.clear();
        return objects_;
    }

protected:
    virtual void print(ostream&) const;
    vector<BasicGraphicsObject*> objects_;
    vector<BasicGraphicsObject*> last_;
};

}  // namespace magics
#endif
