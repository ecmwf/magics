/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*!
    \brief Definition of the Template class ViewNode.

    Magics Team - ECMWF 2007

    Started: Tue 6-Mar-2007

    Changes:

*/

#ifndef PreviewNode_H
#define PreviewNode_H

#include "Coastlines.h"
#include "SceneVisitor.h"
#include "magics.h"


namespace magics {


class NoPreviewVisitor : public SceneVisitor, public PreviewLayout {
public:
    NoPreviewVisitor() {}
    virtual ~NoPreviewVisitor() {}
    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string&) { return false; }
    virtual void toxml(ostream&, int = 0) const {}
    virtual NoPreviewVisitor* clone() const { return new NoPreviewVisitor(); }

    void visit(BasicSceneObject&) {}

protected:
    virtual void print(ostream& s) const;

    friend ostream& operator<<(ostream& s, const NoPreviewVisitor& p) {
        p.print(s);
        return s;
    }
};


class PreviewVisitor : public NoPreviewVisitor {
public:
    PreviewVisitor();
    ~PreviewVisitor();
    NoPreviewVisitor* clone() const { return new PreviewVisitor(); }
    void visit(BasicGraphicsObjectContainer&);
    void redisplay(const BaseDriver& driver) const;
    void visit(BasicSceneObject& object);
    CoastPlotting& coastlines() { return coastlines_; }

protected:
    void print(ostream& s) const;
    CoastPlotting coastlines_;
};


template <>
class MagTranslator<string, NoPreviewVisitor> {
public:
    NoPreviewVisitor* operator()(const string& val) { return SimpleObjectMaker<NoPreviewVisitor>::create(val); }

    NoPreviewVisitor* magics(const string& param) {
        NoPreviewVisitor* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
