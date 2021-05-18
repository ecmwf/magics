/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file VisualAction.h
    \brief Definition of the Template class ActionNode.

    Magics Team - ECMWF 2007

    Started: Tue 6-Mar-2007

    Changes:

*/

#ifndef ActionNode_H
#define ActionNode_H

#include "magics.h"

#include "BasicSceneObject.h"
#include "Data.h"
#include "Visdef.h"

namespace magics {
class FrameLoop;
class HistoVisitor;


class VisualAction : public BasicSceneObject {
public:
    VisualAction();
    virtual ~VisualAction() override;

    void data(Data* data) override { data_ = data; }
    void set2D() override {
        if (data_)
            data_->dimension(2);
    }
    void visdef(Visdef* visdef) override {
        visdef->theme(theme());
        visdefs_.push_back(visdef);
    }
    void binning(BinningObject* binning) override {
        if (data_)
            data_->binning(binning);
    }
    const string name();
    const string id();

    // A visual Action is valid, if the data have been set and the list of visdef is not empty!
    bool isValid() const { return data_ && visdefs_.empty() == false; }

    bool needLegend() override;
    void getReady(const LegendVisitor&) override;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;
    void getReady() override;
    void release() override;

    void visit(DrawingVisitor&) override;
    void visit(TopAxisVisitor&) override;
    void visit(LegendVisitor&) override;
    void visit(TextVisitor&) override;
    void visit(MetaDataVisitor&) override;
    void visit(HistoVisitor&) override;
    void visit(FrameVisitor&) override;

    void visit(Transformation&) override;

    void visit(AnimationRules&) override;

    void visit(SceneLayer&, vector<LayoutVisitor*>&) override;
    void visit(MetaDataCollector&) override;
    void visit(ValuesCollector&) override;
    void visit(DataIndexCollector&) override;
    void visit(MagnifierCollector&) override;

    Data* data_;
    vector<Visdef*> visdefs_;
    StaticLayer* layer_;

    void visit(DateDescription&) override;
    void visit(LevelDescription&) override;

private:
    //! Copy constructor - No copy allowed
    VisualAction(const VisualAction&);
    //! Overloaded << operator to copy - No copy allowed
    VisualAction& operator=(const VisualAction&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const VisualAction& p) {
        p.print(s);
        return s;
    }
};


class VisualAnimation : public VisualAction, public vector<BasicSceneObject*> {
public:
    VisualAnimation();
    virtual ~VisualAnimation() override;
    void loop(DataLoop* data) { loop_ = data; }
    DataLoop& loop() {
        ASSERT(loop_);
        return *loop_;
    }

protected:
    void visit(MetaDataVisitor&) override;
    void visit(Transformation&) override;
    void visit(AnimationRules&) override;
    void visit(SceneLayer&, vector<LayoutVisitor*>&) override;

    void prepare();

    DataLoop* loop_;
    StepLayer* layer_;
};

}  // namespace magics

#endif
