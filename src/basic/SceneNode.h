/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SceneNode.h
    \brief Definition of the Template class SceneNode.
    \author Meteorological Visualisation Section, ECMWF

    Started: Mar-2007

    Changes:

*/

#ifndef SceneNode_H
#define SceneNode_H

#include "magics.h"

#include "BasicSceneObject.h"

#include "FortranSceneNodeAttributes.h"
#include "XmlSceneNodeAttributes.h"

namespace magics {

class SceneNode : public BasicSceneNode {
public:
    SceneNode();
    virtual ~SceneNode();

    BasicSceneNode* clone() { return new SceneNode(); }

    void getReady();
    void text(TextVisitor*);
    void legend(LegendVisitor*);
    void visit(BasicGraphicsObjectContainer& tree);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    vector<TextVisitor*> texts_;
    LegendVisitor* legend_;

private:
    SceneNode(const SceneNode&);
    //! Overloaded << operator to copy - No copy allowed
    SceneNode& operator=(const SceneNode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SceneNode& p) {
        p.print(s);
        return s;
    }
};

class FortranSceneNode : public SceneNode, public FortranSceneNodeAttributes {
public:
    FortranSceneNode();
    ~FortranSceneNode();
    void getReady();
    void resize();
    void visit(BasicGraphicsObjectContainer& tree);
    BasicSceneNode* clone() {
        FortranSceneNode* node = new FortranSceneNode();
        node->copy(*this);
        return node;
    }
    string theme() const;

protected:
    void print(ostream&) const;
    SceneLayer* sceneLayer_;
};


class XmlSceneNode : public SceneNode, public XmlSceneNodeAttributes {
public:
    XmlSceneNode();
    ~XmlSceneNode();
    void set(const map<string, string>& map) { XmlSceneNodeAttributes::set(map); }
    void set(const XmlNode& node) { XmlSceneNodeAttributes::set(node); }
    void getReady();

protected:
    void print(ostream&) const;
};

}  // namespace magics
#endif
