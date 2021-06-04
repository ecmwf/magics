/*Super
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ViewNode.h
    \brief Definition of the Template class ViewNode.

    Magics Team - ECMWF 2007

    Started: Tue 6-Mar-2007

    Changes:

*/

#ifndef ViewNode_H
#define ViewNode_H

#include "BasicSceneObject.h"
#include "FortranViewNodeAttributes.h"
#include "Layout.h"
#include "XmlBasicNodeAttributes.h"
#include "XmlNode.h"
#include "XmlViewNodeAttributes.h"
#include "magics.h"


namespace magics {

class Transformation;
class AnimationRules;

class ViewNode : public BasicSceneNode {
public:
    ViewNode();
    virtual ~ViewNode() override;
    void setInteractiveInfo(const string& id, int levels, int level) {
        name(id);
        ASSERT(layout_);
        layout_->id(id);
        id_               = id;
        zoomLevels_       = levels;
        zoomCurrentLevel_ = level;
        layout_->zoomLevels(levels);
        layout_->zoomCurrentLevel(level);
    }

    virtual void text(TextVisitor* text) override;
    virtual void legend(LegendVisitor* legend) override;


    void visit(MetaDataCollector&) override;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    void print(ostream&) const override;
    void prepareLayout(SceneLayer&);
    virtual void updateLayout() {}

    void getReady() override;

    void visit(SceneLayer&) override;
    void visit(MetaDataVisitor&) override;
    void visit(PreviewVisitor&) override;
    void visit(HistoVisitor&) override;

    virtual void copy(const ViewNode&);

    Transformation& transformation() const override {
        ASSERT(viewTransformation_);
        return *viewTransformation_;
    }

    Transformation* viewTransformation_;

    // The margin are used to position the drawing area, inside the node!
    double drawing_top_;  // In automatic mode : The top should always be greater that 15% to allow a space for the
                          // automatic title, and automatic legend!
    double drawing_bottom_;
    double drawing_left_;
    double drawing_right_;
    string animation_;

    DrawingVisitor* drawing_;
    FrameVisitor* frameHelper_;
    TopAxisVisitor* topAxis_;
    BottomAxisVisitor* bottomAxis_;
    LeftAxisVisitor* leftAxis_;
    RightAxisVisitor* rightAxis_;
    double vaxis_;
    double haxis_;

    AnimationRules* rules_;

    bool needLegend_;
    LegendVisitor* legend_;
    vector<TextVisitor*> texts_;
    vector<LayoutVisitor*> components_;

    string drawing_background_colour_;
    bool frameIt_;
    Colour frameColour_;
    LineStyle frameStyle_;
    int frameThickness_;

    // Infomation about the zoom!
    string id_;
    int zoomLevels_;
    int zoomCurrentLevel_;


private:
    //! Overloaded << operator to copy - No copy allowed
    ViewNode(const ViewNode&);
    //! Overloaded << operator to copy - No copy allowed
    ViewNode& operator=(const ViewNode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ViewNode& p) {
        p.print(s);
        return s;
    }
};

class FortranViewNode : public ViewNode, public FortranViewNodeAttributes {
public:
    FortranViewNode();
    ~FortranViewNode() override;

    BasicSceneNode* clone() override;

    void getReady() override;


protected:
    void print(ostream&) const override;
};

class XmlViewNode : public ViewNode, public XmlBasicNodeAttributes, public XmlViewNodeAttributes {
public:
    XmlViewNode();
    ~XmlViewNode() override;
    void set(const XmlNode& node) override {
        if (magCompare(node.name(), "map")) {
            XmlNode view = node;
            view.name("view");
            XmlBasicNodeAttributes::set(view);
        }
        XmlViewNodeAttributes::set(node);
    }


    void updateLayout() override;
    void getReady() override;

protected:
    void print(ostream&) const override;
};


}  // namespace magics
#endif
