/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file InteractiveSet.h
    \brief Implementation of InteractiveSet class.

    Magics Team - ECMWF 2004

    Started: March 2004

    Changes:

*/
#ifndef InteractiveSet_H
#define InteractiveSet_H

#include "BasicGraphicsObject.h"


namespace magics {

class InteractiveAction {
public:
    InteractiveAction() {}
    virtual ~InteractiveAction() override {}

    virtual void execute() {}  // protype to be confirmed!
protected:
    virtual void print(ostream& s) const override { s << "InteractiveAction[]"; }
    friend ostream& operator<<(ostream& s, const InteractiveAction& p) {
        p.print(s);
        return s;
    }
};

class InteractiveBegin : public BasicGraphicsObject, public map<string, InteractiveAction*> {
public:
    InteractiveBegin(Layer* layer = 0, const string& box = "");
    ~InteractiveBegin() override {}

    bool reproject(const Transformation& transformation, BasicGraphicsObjectContainer& out) const;

    virtual void redisplay(const BaseDriver&) const override;

protected:
    virtual void print(ostream&) const override;
};

class InteractiveEnd : public BasicGraphicsObject {
public:
    InteractiveEnd(Layer* layer = 0, const string& box = "");
    ~InteractiveEnd() override {}

    virtual void redisplay(const BaseDriver&) const override;
    bool reproject(const Transformation& transformation, BasicGraphicsObjectContainer& out) const;

protected:
    virtual void print(ostream&) const override;
};


class InteractiveSet : public BasicGraphicsObject, public vector<BasicGraphicsObject*> {
public:
    InteractiveSet(Layer* layer = 0, const string& box = "");
    virtual ~InteractiveSet() override {}

    virtual bool reproject(const Transformation&, BasicGraphicsObjectContainer&) const override;
    virtual void redisplay(const BaseDriver&) const override;
    void addAction(const string& name, InteractiveAction* action) { begin_->insert(make_pair(name, action)); }

protected:
    virtual void print(ostream&) const override;
    Layer* layer_;
    string box_;
    mutable Label* label_;
    Colour colour_;
    bool toBeDeleted_;
    InteractiveBegin* begin_;
    InteractiveEnd* end_;

private:
    // No copy allowed
    InteractiveSet& operator=(const InteractiveSet&);

    // -- Friends
    friend ostream& operator<<(ostream& s, const InteractiveSet& p) {
        p.print(s);
        return s;
    }
};


class InteractiveLink : public InteractiveAction {
public:
    InteractiveLink(const string& url = "http://www.ecmwf.int") : url_(url) {}
    ~InteractiveLink() override {}
    void url(const string& url) { url_ = url; }
    const string& url() const { return url_; }

protected:
    string url_;
    virtual void print(ostream&) const override;
};

class InteractiveMagnify : public InteractiveAction {
public:
    InteractiveMagnify(float factor = 2) : factor_(factor) {}
    ~InteractiveMagnify() override {}
    void factor(float factor) { factor_ = factor; }
    float factor() const { return factor_; }

protected:
    float factor_;
    virtual void print(ostream&) const override;
};


}  // namespace magics
#endif
