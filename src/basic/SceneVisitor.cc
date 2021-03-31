/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file SceneVisitor.cc
    \brief Implementation of the Template class SceneVisitor.

    Magics Team - ECMWF 2008

    Started: Mon 29-Dec-2008

    Changes:

*/

#include "SceneVisitor.h"

#include "AnimationRules.h"
#include "BaseDriver.h"
#include "Layer.h"
#include "Transformation.h"

using namespace magics;

SceneVisitor::SceneVisitor() {}

SceneVisitor::~SceneVisitor() {}

bool SceneVisitor::reproject(BasicGraphicsObjectContainer&) const {
    return true;
}

void LayoutVisitor::redisplay(const BaseDriver& driver) const {
    driver.redisplay(*layout_);
    driver.redisplay(*current_);
}

/*!
 Class information are given to the output-stream.
*/
void SceneVisitor::print(ostream& out) const {
    out << "SceneVisitor[";
    out << "]";
}

DrawingVisitor::DrawingVisitor() {
    layout_ = new Layout();
    layout_->name("drawing");
    layout_->setNavigable();
}

DrawingVisitor::~DrawingVisitor() {}

void LayoutVisitor::print(ostream& out) const {
    out << "LayoutVisitor[";
    out << *layout_;
    out << "]";
}

HorizontalAxisVisitor::HorizontalAxisVisitor(const DrawingVisitor&) {}

HorizontalAxisVisitor::~HorizontalAxisVisitor() {}
FrameVisitor::FrameVisitor() {
    layout_ = new Layout();
    layout_->name("frame");
    background_ = "white";
}

FrameVisitor::~FrameVisitor() {}

VerticalAxisVisitor::VerticalAxisVisitor(const DrawingVisitor&) {}

VerticalAxisVisitor::~VerticalAxisVisitor() {}

void VerticalAxisVisitor::tick(double&, double&, bool) {}

void VerticalAxisVisitor::minortick(double&, double&, bool) {}
double VerticalAxisVisitor::offsetTickLabel(double, double) {
    return 0;
}
double VerticalAxisVisitor::angleTickLabel() {
    return 0;
}
double VerticalAxisVisitor::angleTitle() {
    return 0;
}
double VerticalAxisVisitor::offsetTitle(int) {
    return 0;
}
double VerticalAxisVisitor::angleTip() {
    return 0;
}
double VerticalAxisVisitor::offsetTip() {
    return 0;
}

void HorizontalAxisVisitor::tick(double&, double&, bool) {}
void HorizontalAxisVisitor::minortick(double&, double&, bool) {}
double HorizontalAxisVisitor::offsetTickLabel(double, double) {
    return 0;
}
double HorizontalAxisVisitor::angleTickLabel() {
    return 0;
}
double HorizontalAxisVisitor::angleTitle() {
    return 0;
}
double HorizontalAxisVisitor::offsetTitle(int) {
    return 0;
}
double HorizontalAxisVisitor::angleTip() {
    return 0;
}
double HorizontalAxisVisitor::offsetTip() {
    return 0;
}

TopAxisVisitor::TopAxisVisitor(const DrawingVisitor& drawing) : HorizontalAxisVisitor(drawing) {
    layout_ = new Layout();
    layout_->name("top");
    drawing.transformation(*layout_);
}

TopAxisVisitor::~TopAxisVisitor() {}

BottomAxisVisitor::BottomAxisVisitor(const DrawingVisitor& drawing) : HorizontalAxisVisitor(drawing) {
    layout_ = new Layout();
    layout_->name("bottom");
    drawing.transformation(*layout_);
}

BottomAxisVisitor::~BottomAxisVisitor() {}

RightAxisVisitor::RightAxisVisitor(const DrawingVisitor& drawing) : VerticalAxisVisitor(drawing) {
    layout_ = new Layout();
    layout_->name("right");
    drawing.transformation(*layout_);
}

RightAxisVisitor::~RightAxisVisitor() {}

LeftAxisVisitor::LeftAxisVisitor(const DrawingVisitor& drawing) : VerticalAxisVisitor(drawing) {
    layout_ = new Layout();
    layout_->name("left");
    drawing.transformation(*layout_);
}

LeftAxisVisitor::~LeftAxisVisitor() {}

void LeftAxisVisitor::tick(double& x1, double& x2, bool) {
    double length = ((current_->xmax_ - current_->xmin_) / current_->absoluteWidth()) * 0.2;
    x2            = current_->xmax_;
    x1            = current_->xmax_ - length;
}

void LeftAxisVisitor::minortick(double& x1, double& x2, bool) {
    double length = ((current_->xmax_ - current_->xmin_) / current_->absoluteWidth()) * 0.1;
    x2            = current_->xmax_;
    x1            = current_->xmax_ - length;
}

double LeftAxisVisitor::offsetTickLabel(double height, double from) {
    double h = ((current_->xmax_ - current_->xmin_) / current_->absoluteWidth()) * (height * 0.6);

    return from - h;
}

double LeftAxisVisitor::shiftTitle(double position) {
    double shift = (current_->xmax_ - current_->xmin_) * 0.1;

    return position - shift;
}

double LeftAxisVisitor::angleTickLabel() {
    return 0;
}

double LeftAxisVisitor::angleTitle() {
    return 3 * 3.14 / 2;
}

double LeftAxisVisitor::offsetTitle(int) {
    return current_->xmin_;
}
double LeftAxisVisitor::angleTip() {
    return 3 * 3.14 / 2;
}

double LeftAxisVisitor::offsetTip() {
    return current_->xmin_;
}

void RightAxisVisitor::tick(double& x1, double& x2, bool) {
    double length = ((current_->xmax_ - current_->xmin_) / current_->absoluteWidth()) * 0.2;
    x2            = current_->xmin_;
    x1            = x2 + length;
}

void RightAxisVisitor::minortick(double& x1, double& x2, bool) {
    double length = ((current_->xmax_ - current_->xmin_) / current_->absoluteWidth()) * 0.1;
    x2            = current_->xmin_;
    x1            = x2 + length;
}

double RightAxisVisitor::offsetTickLabel(double height, double from) {
    double h = ((current_->xmax_ - current_->xmin_) / current_->absoluteWidth()) * (2 * height);
    return from + h;
}

double RightAxisVisitor::shiftTitle(double position) {
    double shift = (current_->xmax_ - current_->xmin_) * 0.1;

    return position + shift;
}

double RightAxisVisitor::angleTickLabel() {
    return 0;
}

double RightAxisVisitor::angleTitle() {
    return 3.14 / 2;
}

double RightAxisVisitor::offsetTitle(int) {
    return current_->xmax_;
}
double RightAxisVisitor::angleTip() {
    return 3.14 / 2;
}

double RightAxisVisitor::offsetTip() {
    return current_->xmax_;
}

void TopAxisVisitor::tick(double& y1, double& y2, bool out) {
    double length = ((current_->ymax_ - current_->ymin_) / current_->absoluteHeight()) * 0.15;
    y1            = current_->ymin_;
    y2            = (out) ? y2 + length : y2 - length;
}

void TopAxisVisitor::minortick(double& y1, double& y2, bool out) {
    double length = ((current_->ymax_ - current_->ymin_) / current_->absoluteHeight()) * 0.1;
    y2            = current_->ymin_;
    y1            = (out) ? y2 + length : y2 - length;
}

double TopAxisVisitor::offsetTickLabel(double height, double from) {
    double h = ((current_->ymax_ - current_->ymin_) / current_->absoluteHeight()) * height;
    return from + h;
}

double TopAxisVisitor::angleTickLabel() {
    return 0;
}

double TopAxisVisitor::angleTitle() {
    return 0;
}

double TopAxisVisitor::offsetTitle(int) {
    return current_->ymax_;
}

double TopAxisVisitor::angleTip() {
    return 0;
}

double TopAxisVisitor::offsetTip() {
    return current_->ymax_;
}

void BottomAxisVisitor::tick(double& y1, double& y2, bool out) {
    ASSERT(current_);
    double length = ((current_->ymax_ - current_->ymin_) / current_->absoluteHeight()) * 0.15;
    y2            = current_->ymax_;
    y1            = (out) ? y2 - length : y2 + length;
}

void BottomAxisVisitor::minortick(double& y1, double& y2, bool out) {
    double length = ((current_->ymax_ - current_->ymin_) / current_->absoluteHeight()) * 0.1;
    y2            = current_->ymax_;
    y1            = (out) ? y2 - length : y2 + length;
}

double BottomAxisVisitor::offsetTickLabel(double height, double from) {
    double h = ((current_->ymax_ - current_->ymin_) / current_->absoluteHeight()) * height;

    return from - h;
}

double BottomAxisVisitor::angleTickLabel() {
    return 0;
}

double BottomAxisVisitor::angleTitle() {
    return 0;
}

double BottomAxisVisitor::offsetTitle(int) {
    return current_->ymin_;
}

double BottomAxisVisitor::angleTip() {
    return 0;
}

double BottomAxisVisitor::offsetTip() {
    return current_->ymin_;
}

void LayoutVisitor::push_back(BasicGraphicsObject* object) {
    ASSERT(current_);
    current_->push_back(object);
}
void DrawingVisitor::set(MagnifierCollector& magnifier) {
    newLayout();

    magnifier.setLayout(current_);
}

void LayoutVisitor::newLayout() const {
    current_ = layout_->clone();
}

Layout& LayoutVisitor::layout() const {
    if (!current_)
        newLayout();
    return *current_;
}
Layout* LayoutVisitor::layoutPtr() const {
    ASSERT(layout_);
    return current_;
}
Layout* LayoutVisitor::mainLayout() const {
    ASSERT(layout_);
    return layout_;
}

Justification TopAxisVisitor::justificationTickLabel(const string& orientation) {
    // horizontal == parallel
    return (magCompare(orientation, "vertical")) ? MLEFT : MCENTRE;
}

Justification BottomAxisVisitor::justificationTickLabel(const string& orientation) {
    // horizontal == parallel
    return (magCompare(orientation, "vertical")) ? MRIGHT : MCENTRE;
}

Justification LeftAxisVisitor::justificationTickLabel(const string& orientation) {
    // vertical == parallel
    return (magCompare(orientation, "horizontal")) ? MRIGHT : MCENTRE;
}
Justification RightAxisVisitor::justificationTickLabel(const string& orientation) {
    // vertical == parallel
    return (magCompare(orientation, "horizontal")) ? MLEFT : MCENTRE;
}
Justification VerticalAxisVisitor::justificationTickLabel(const string& orientation) {
    return MCENTRE;
}

VerticalAlign TopAxisVisitor::textAlignment(const string& orientation) {
    // horizontal == parallel
    return (magCompare(orientation, "vertical")) ? MHALF : MTOP;
}

VerticalAlign BottomAxisVisitor::textAlignment(const string& orientation) {
    // horizontal == parallel
    return (magCompare(orientation, "vertical")) ? MHALF : MBOTTOM;
}

VerticalAlign LeftAxisVisitor::textAlignment(const string& orientation) {
    // vertical == parallel
    return (magCompare(orientation, "horizontal")) ? MHALF : MBOTTOM;
}
VerticalAlign RightAxisVisitor::textAlignment(const string& orientation) {
    // vertical == parallel
    return (magCompare(orientation, "horizontal")) ? MHALF : MTOP;
}

double RightAxisVisitor::percentX(double percent) {
    double pos = ((current_->xmax_ - current_->xmin_) / 100) * percent;
    return current_->xmin_ + pos;
}
double RightAxisVisitor::percentY(double percent) {
    double pos = ((current_->ymax_ - current_->ymin_) / 100) * percent;
    return current_->ymin_ + pos;
}

double LeftAxisVisitor::percentX(double percent) {
    double pos = ((current_->xmax_ - current_->xmin_) / 100) * percent;
    return current_->xmax_ - pos;
}
double LeftAxisVisitor::percentY(double percent) {
    double pos = ((current_->ymax_ - current_->ymin_) / 100) * percent;
    return current_->ymin_ + pos;
}

double BottomAxisVisitor::percentX(double percent) {
    double pos = ((current_->xmax_ - current_->xmin_) / 100) * percent;
    return current_->xmin_ + pos;
}
double BottomAxisVisitor::percentY(double percent) {
    double pos = ((current_->ymax_ - current_->ymin_) / 100) * percent;
    return current_->ymin_ + pos;
}

double TopAxisVisitor::percentX(double percent) {
    double pos = ((current_->xmax_ - current_->xmin_) / 100) * percent;
    return current_->xmin_ + pos;
}
double TopAxisVisitor::percentY(double percent) {
    double pos = ((current_->ymax_ - current_->ymin_) / 100) * percent;
    return current_->ymin_ + pos;
}