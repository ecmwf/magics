/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file OriginMarker.h
    \brief Definition of the Template class OriginMarker.

    Magics Team - ECMWF 2005

    Started: Thu 17-Mar-2005

    Changes:

*/

#ifndef OriginMarker_H
#define OriginMarker_H

#include "Arrow.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "Transformation.h"
#include "magics.h"


namespace magics {

class OriginMarker {
public:
    OriginMarker() : marker_("none"), height_(0) {}
    virtual ~OriginMarker() {}
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }

    virtual void toxml(ostream&) {}
    virtual OriginMarker* clone() {
        OriginMarker* object = new OriginMarker();
        object->set(*this);
        return object;
    }

    void marker(const string& marker) { marker_ = marker; }
    void height(double height) { height_ = height; }
    virtual double height() { return height_; }
    void set(const OriginMarker& from) {
        marker_ = from.marker_;
        height_ = from.height_;
    }
    virtual void prepare(ArrowProperties& object) {
        object.setOriginMarker(marker_);
        object.setOriginHeight(height());
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "OriginMarker"; }

    string marker_;
    double height_;

private:
    //! Copy constructor - No copy allowed
    OriginMarker(const OriginMarker&);
    //! Overloaded << operator to copy - No copy allowed
    OriginMarker& operator=(const OriginMarker&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const OriginMarker& p) {
        p.print(s);
        return s;
    }
};


class NoOriginMarker : public OriginMarker {
public:
    NoOriginMarker() {}
    ~NoOriginMarker() {}
    virtual OriginMarker* clone() { return new NoOriginMarker(); }
    virtual void prepare(ArrowProperties& object) { object.setOriginHeight(0); }


    void operator()(const PaperPoint&) {}
    virtual bool accept(const string& node) { return magCompare(node, "nomarker"); }
    double height() { return 0.; }
};


class CircleOriginMarker : public OriginMarker {
public:
    CircleOriginMarker() {
        this->marker_ = "magics_1";
        this->height_ = 0.3;
    }
    virtual OriginMarker* clone() {
        OriginMarker* object = new CircleOriginMarker();
        object->set(*this);
        return object;
    }
    virtual bool accept(const string& node) { return magCompare(node, "circlemarker"); }

    ~CircleOriginMarker() {}
};


class DotOriginMarker : public OriginMarker {
public:
    DotOriginMarker() {
        this->marker_ = "magics_15";
        this->height_ = 0.1;
    }
    virtual OriginMarker* clone() {
        OriginMarker* object = new DotOriginMarker();
        object->set(*this);
        return object;
    }
    ~DotOriginMarker() {}
    virtual bool accept(const string& node) { return magCompare(node, "dotmarker"); }
};

template <>
class MagTranslator<string, OriginMarker> {
public:
    OriginMarker* operator()(const string& val) { return SimpleObjectMaker<OriginMarker>::create(val); }
    OriginMarker* magics(const string& param) {
        OriginMarker* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
