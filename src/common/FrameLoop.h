/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file FrameLoop.h
    \brief Definition of the Template class FrameLoop.

    Magics Team - ECMWF 2008

    Started: Fri 29-Aug-2008

    Changes:

*/

#ifndef FrameLoop_H
#define FrameLoop_H

#include "magics.h"

#include "BaseGraphicsObject.h"
#include "BasicSceneObject.h"
#include "TagHandler.h"

namespace magics {

class AnimationRules;
class AsIsAnimationRules;

class FrameEntry : public BasicSceneObject, public GraphicsList {
public:
    FrameEntry();
    virtual ~FrameEntry();
    void execute(const BaseDriver&);
    void add(BasicSceneObject* object) { BasicSceneObject::push_back(object); }
    void add(BaseGraphicsObject* object) { GraphicsList::push_back(object); }
    void visit(AnimationRules&);
    void tag(AnimationRules&);
    void animate(AsIsAnimationRules&);
    string label() const { return label_; }
    void index(int index) { index_ = index; }

protected:
    void print(ostream&) const;
    string label_;
    int index_;
};


class FrameLoop : public BaseGraphicsObject, public vector<FrameEntry*> {
public:
    FrameLoop();
    virtual ~FrameLoop();
    bool reproject(const Transformation&, BaseGraphicsList& out) const;
    void redisplay(const BaseDriver& driver) const;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    AnimationRules* rules_;  // Later will probably come from an Attributes!

private:
    //! Copy constructor - No copy allowed
    FrameLoop(const FrameLoop&);
    //! Overloaded << operator to copy - No copy allowed
    FrameLoop& operator=(const FrameLoop&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const FrameLoop& p) {
        p.print(s);
        return s;
    }
};

class AnimationStep : public vector<FrameEntry*> {
public:
    AnimationStep();
    virtual ~AnimationStep();
    string label() { return label_; }
    void label(const string& label) { label_ = label; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string label_;

private:
    //! Copy constructor - No copy allowed
    AnimationStep(const AnimationStep&);
    //! Overloaded << operator to copy - No copy allowed
    AnimationStep& operator=(const AnimationStep&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AnimationStep& p) {
        p.print(s);
        return s;
    }
};


class AnimationRules : public TagHandler, public vector<AnimationStep*> {
public:
    AnimationRules();
    virtual ~AnimationRules();
    virtual void callback(FrameEntry&){};

    string labelFormat() const { return labelFormat_; }

    virtual void getReady() {}
    virtual void rules(vector<string>&) const;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string labelFormat_;

private:
    //! Copy constructor - No copy allowed
    AnimationRules(const AnimationRules&);
    //! Overloaded << operator to copy - No copy allowed
    AnimationRules& operator=(const AnimationRules&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AnimationRules& p) {
        p.print(s);
        return s;
    }
};


class AsIsAnimationRules : public AnimationRules {
public:
    AsIsAnimationRules();
    virtual ~AsIsAnimationRules();
    virtual void callback(FrameEntry& entry) { entry.animate(*this); }
    AnimationStep* step(int);
    void getReady();
    void rules(vector<string>&) const;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    map<int, AnimationStep*> steps_;
};
class DateAnimationRules : public AnimationRules {
public:
    DateAnimationRules();
    virtual ~DateAnimationRules();


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
};

}  // namespace magics
#endif
