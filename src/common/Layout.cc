/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Layout.cc
    \brief Implementation of the Template class Layout.

    Magics Team - ECMWF 2004

    Started: Fri 30-Jan-2004

    Changes:

*/

#include "Layout.h"
#include "BaseDriver.h"

#include "PaperPoint.h"
#include "Polyline.h"
#include "SceneVisitor.h"

#include "Transformation.h"


using namespace magics;

const double axisHeight_   = 3;
const double axisWidth_    = 3;
const double legendHeight_ = 3;

#define CLEAR(a) \
    if (a)       \
        delete a;

Layout::Layout() :
    owner_(0),
    animationRules_(0),
    transformation_(0),
    width_(100),
    height_(100),
    x_(0),
    y_(0),
    display_(DisplayType::INLINE),
    xmin_(0),
    xmax_(100),
    ymin_(0),
    ymax_(100),
    zoomable_(false),
    navigable_(false),
    resizable_(false),
    resolve_(false),
    clipping_(false) {}

Layout::~Layout() {
    if (owner_)
        delete owner_;
}

void Layout::redisplay(const BaseDriver& driver) const {
    if (objects_.empty() && name_ != "drawing")
        return;
    MagLog::debug() << "Layout::redisplay-->" << *this << endl;
    driver.redisplay(*this);
}

bool Layout::reproject(BasicGraphicsObjectContainer& /*out*/) const {
    // out.push_back(this));
    return true;
}

/*!
 Class information are given to the output-stream.
*/
void Layout::print(ostream& out) const {
    out << "Layout[";
    out << name_;
    if (parent_)
        out << ", parent=" << parent_->name();
    out << ", x=" << x_;
    out << ", y=" << y_;
    out << ", width=" << width_;
    out << ", height=" << height_;
    out << ", xmin=" << xmin_;
    out << ", ymin=" << ymin_;
    out << ", xmax=" << xmax_;
    out << ", ymax=" << ymax_;
    out << ", size=" << objects_.size();
    out << "]";
    for (vector<DriverInfo>::const_iterator info = driverInfos_.begin(); info != driverInfos_.end(); ++info)
        out << *info << endl;
}


double Layout::absoluteWidth(double width) {
    return width_ = 100 * width / absoluteWidth();
}

double Layout::absoluteHeight(double height) {
    return height_ = 100 * height / absoluteHeight();
}

RootLayout::RootLayout(double width, double height) : absoluteWidth_(width), absoluteHeight_(height) {}

RootLayout::~RootLayout() {}

PreviewLayout::~PreviewLayout() {}

PreviewLayout::PreviewLayout() {}

MagnifierLayout::~MagnifierLayout() {}

MagnifierLayout::MagnifierLayout() {}


LayoutFrame::LayoutFrame() :
    thickness_(1), style_(LineStyle::SOLID), colour_("grey"), blanking_(false), visible_(false) {}

LayoutFrame::~LayoutFrame() {}

Layout* Layout::execute(AnimationStep& /*step*/, const Layout* /*visitor*/) {
    return 0;
}

void LayoutFrame::blank(Layout& owner, const string& colour) {
    if (blanking_) {
        // Create and push_back the frame!

        Polyline* frame = new Polyline();
        frame->setLineStyle(style_);
        frame->setThickness(thickness_);
        frame->setColour(visible_ ? colour_ : Colour("none"));
        frame->setFilled(true);
        frame->setFillColour(colour);


        FillShadingProperties* shading = new FillShadingProperties();

        frame->setShading(shading);

        frame->push_back(PaperPoint(owner.minX(), owner.minY()));
        frame->push_back(PaperPoint(owner.minX(), owner.maxY()));
        frame->push_back(PaperPoint(owner.maxX(), owner.maxY()));
        frame->push_back(PaperPoint(owner.maxX(), owner.minY()));
        frame->push_back(PaperPoint(owner.minX(), owner.minY()));

        owner.push_back(frame);
    }
}

void LayoutFrame::frame(Layout& owner) {
    if (!visible_)
        return;
    // Create and push_back the frame!

    Polyline* frame = new Polyline();
    frame->setLineStyle(style_);
    frame->setThickness(thickness_);
    frame->setColour(colour_);


    double px = (owner.maxX() - owner.minX()) * 0.00;
    double py = (owner.maxY() - owner.minY()) * 0.00;


    frame->push_back(PaperPoint(owner.minX() + px, owner.minY() + py));
    frame->push_back(PaperPoint(owner.minX() + px, owner.maxY() - py));
    frame->push_back(PaperPoint(owner.maxX() - px, owner.maxY() - py));
    frame->push_back(PaperPoint(owner.maxX() - px, owner.minY() + py));
    frame->push_back(PaperPoint(owner.minX() + px, owner.minY() + py));

    owner.push_back(frame);
}

double Layout::absoluteX() const {
    // return parent_->absoluteX() + x_ * parent_->absoluteWidth() /100;
    return 0;
}

double Layout::absoluteY() const {
    // return parent_->absoluteY() + y_ * parent_->absoluteHeight() /100;
    return 0;
}

double Layout::absoluteWidth() const {
    ASSERT(parent_);
    return width_ * parent_->absoluteWidth() / 100;
}

double Layout::absoluteHeight() const {
    ASSERT(parent_);
    return height_ * parent_->absoluteHeight() / 100;
}


void Layout::transformation(Transformation* transformation) {
    transformation_ = transformation;
    xmin_           = transformation_->getMinPCX();
    xmax_           = transformation_->getMaxPCX();
    ymin_           = transformation_->getMinPCY();
    ymax_           = transformation_->getMaxPCY();
}

void Layout::redisplay(AnimationStep& step, const BaseDriver& driver) {
    BasicGraphicsObject* plot = owner_->execute(step, *this);

    if (plot)
        plot->redisplay(driver);
}


LayoutHelper::LayoutHelper() : xmin_(INT_MAX), xmax_(-INT_MAX), ymin_(INT_MAX), ymax_(-INT_MAX) {}

LayoutHelper::~LayoutHelper() {}

void LayoutHelper::add(LayoutVisitor* visitor) {
    Layout* layout = visitor->mainLayout();
    if (xmin_ > layout->x())
        xmin_ = layout->x();
    if (ymin_ > layout->y())
        ymin_ = layout->y();
    if (xmax_ < (layout->x() + layout->width()))
        xmax_ = layout->x() + layout->width();
    if (ymax_ < (layout->y() + layout->height()))
        ymax_ = layout->y() + layout->height();
    MagLog::dev() << "New Layout-->" << *this << endl;
}

void LayoutHelper::attachTop(LayoutVisitor* visitor) {
    Layout* layout = visitor->mainLayout();
    layout->y(ymax_);
    layout->x(xmin_);
    layout->width(xmax_ - xmin_);
}

void LayoutHelper::attachNoConstraintTop(LayoutVisitor* visitor) {
    Layout* layout = visitor->mainLayout();
    layout->y(ymax_);
    layout->x(xmin_);
}

void LayoutHelper::attachLeft(LayoutVisitor* visitor) {
    Layout* layout = visitor->mainLayout();
    layout->x(xmin_ - layout->width());
    layout->y(ymin_);
    layout->height(ymax_ - ymin_);
}

void LayoutHelper::attachRight(LayoutVisitor* visitor) {
    Layout* layout = visitor->mainLayout();
    layout->x(xmax_);
    layout->y(ymin_);
    layout->height(ymax_ - ymin_);
}

void LayoutHelper::attachBottom(LayoutVisitor* visitor) {
    Layout* layout = visitor->mainLayout();
    // if ( ymin_ - layout->height() > 0 )
    layout->y(ymin_ - layout->height());
    // else {
    //	layout->y(0);

    // layout->height(ymin_);
    //}
    layout->x(xmin_);
    layout->width(xmax_ - xmin_);
}

void LayoutHelper::print(ostream& out) const {
    out << "LayoutHelper[";
    out << "xmin_=" << xmin_;
    out << "xmax_=" << xmax_;
    out << "ymin_=" << ymin_;
    out << "ymax_=" << ymax_;
    out << "]";
}

Layout* Layout::clone() const {
    Layout* layout = newLayout();

    layout->name_   = name_;
    layout->width_  = width_;
    layout->height_ = height_;
    layout->x_      = x_;
    layout->y_      = y_;

    layout->xmin_           = xmin_;
    layout->xmax_           = xmax_;
    layout->ymin_           = ymin_;
    layout->ymax_           = ymax_;
    layout->transformation_ = transformation_;
    layout->parent_         = parent_;

    layout->id_               = id_;
    layout->zoomable_         = zoomable_;
    layout->navigable_        = navigable_;
    layout->zoomLevels_       = zoomLevels_;
    layout->zoomCurrentLevel_ = zoomCurrentLevel_;
    layout->widthResolution_  = widthResolution_;
    layout->heightResolution_ = heightResolution_;
    layout->resizable_        = resizable_;
    layout->clipping_         = clipping_;

    layout->frame(*this);

    return layout;
}

void RootLayout::redisplay(const BaseDriver& driver) const {
    if (resolve_) {
        intarray frames      = driver.frames();
        unsigned int nb      = frames.size();
        unsigned int current = 0;
        bool more            = true;

        while (more) {
            unsigned int frame = (nb) ? frames[current] - 1 : current;
            more               = buildTree(*this, frame, driver);

            current++;
            if (current == nb)
                more = false;
        }
    }
    else
        driver.redisplay(*this);
}

void PreviewLayout::redisplay(const BaseDriver& driver) const {
    driver.redisplay(*this);
}

LegendLayout::LegendLayout() {}

LegendLayout::~LegendLayout() {}

void LegendLayout::redisplay(const BaseDriver& driver) const {
    driver.redisplay(*this);
}

void MagnifierLayout::redisplay(const BaseDriver& driver) const {
    driver.redisplay(*this);
}

void StartPage::redisplay(const BaseDriver& driver) const {
    MagLog::dev() << "StartPage::redisplay-->" << *this << endl;

    driver.redisplay(*this);
}
void EndPage::redisplay(const BaseDriver& driver) const {
    MagLog::dev() << "EndPage::redisplay-->" << *this << endl;

    driver.redisplay(*this);
}
HistoLayout::HistoLayout() {}

HistoLayout::~HistoLayout() {}

void HistoLayout::redisplay(const BaseDriver& driver) const {
    driver.redisplay(*this);
}
SceneLayout::SceneLayout() {}

SceneLayout::~SceneLayout() {}

void SceneLayout::redisplay(const BaseDriver& driver) const {
    if (objects_.empty())
        return;
    MagLog::debug() << "Layout::redisplay-->" << *this << endl;

    driver.redisplay(*this);
}
bool Layout::buildTree(const Layout& parent, unsigned int frame, const BaseDriver& driver) const {
    bool more = false;

    for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object != objects_.end(); ++object) {
        if ((*object)->buildTree(*this, frame, driver))
            more = true;
    }


    return more;
}

void Layout::release() {
    if (resolve_) {
        for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object != objects_.end(); ++object)
            (*object)->release();
    }
}

BasicLayout::BasicLayout() {}
BasicLayout::~BasicLayout() {}


bool BasicLayout::buildTree(const Layout& parent, unsigned int frame, const BaseDriver& driver) const {
    bool more = false;


    StartPage* start = new StartPage();
    driver.redisplay(*start);

    for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object != objects_.end(); ++object) {
        if ((*object)->buildTree(*this, frame, driver))
            more = true;
    }

    EndPage* end = new EndPage();
    driver.redisplay(*end);
    return more;
}

string Layout::infoTransformation() const { 
    return transformation_ ? transformation_->name() : "";
}


vector<DriverInfo> Layout::driverInfos_;
