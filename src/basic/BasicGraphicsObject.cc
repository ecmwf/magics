/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File BasicGraphicsObject.cc
// Magics Team - ECMWF 2004

#include "BasicGraphicsObject.h"
#include "BaseDriver.h"

using namespace magics;
BasicGraphicsObject::BasicGraphicsObject() : parent_(0) {}

BasicGraphicsObject::~BasicGraphicsObject() {}

void BasicGraphicsObject::print(ostream& out) const {
    out << "BasicGraphicsObject[";
    out << "]";
}

void BasicGraphicsObjectContainer::print(ostream& out) const {
    out << "BasicGraphicsObjectContainer[";
    for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object != objects_.end(); ++object)
        out << **object << endl;
    out << "]";
}


bool BasicGraphicsObjectContainer::buildTree(const Layout& parent, unsigned int frame, const BaseDriver& driver) const {
    bool more = false;
    for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object != objects_.end(); ++object) {
        if ((*object)->buildTree(parent, frame, driver))
            more = true;
    }
    return more;
}

void BasicGraphicsObjectContainer::visit(const BaseDriver& driver) const {
    for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object != objects_.end(); ++object)
        (*object)->redisplay(driver);
    for (vector<BasicGraphicsObject*>::const_iterator object = last_.begin(); object != last_.end(); ++object)
        (*object)->redisplay(driver);
}

BasicGraphicsObjectContainer::~BasicGraphicsObjectContainer() {
    for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
        if (*object)
            delete *object;
        *object = 0;
    }
    objects_.clear();
}

void BasicGraphicsObjectContainer::clear() {
    objects_.erase(objects_.begin(), objects_.end());
}
void BasicGraphicsObjectContainer::release() {
    MagLog::debug() << "CLEAR CONTAINER" << objects_.size() << endl;
    for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object) {
        (*object)->release();
    }
    objects_.clear();
}
void BasicGraphicsObject::check() {
    if (parent_)
        parent_->remove(this);
}
