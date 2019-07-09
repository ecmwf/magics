/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file RootSceneNode.cc
    \brief Implementation of the Template class RootSceneNode.

    Magics Team - ECMWF 2007

    Started: Thu 1-Mar-2007

    Changes:

*/


#include "RootSceneNode.h"
//#include "DisplayManager.h"
#include "LayoutManager.h"
#include "PaperDimension.h"
#include "SceneNode.h"
#include "Timer.h"


using namespace magics;

void RootSceneNode::setPage(Layout& layout) {
    if (newpage_) {
        if (endpage_)
            layout.push_back(new EndPage());
        layout.push_back(new StartPage());
        endpage_ = true;
        newpage_ = false;
    }
}

BasicGraphicsObject* RootSceneNode::close() {
    BasicGraphicsObject* endpage = 0;
    if (endpage_)
        endpage = new EndPage();
    endpage_ = false;

    return endpage;
}

void RootSceneNode::setPage(RootScenePage* node) {
    current_ = node;
    current_->root(this);
    current_->getReady();


    // current_ is a copy of the basic SceneNode with exactly the same dimensions as the Root!
    current_->parent(this);
    items_.push_back(current_);
}

void FortranRootSceneNode::setPage(RootScenePage* node) {
    current_ = node;
    current_->root(this);
    current_->getReady();

    // current_ is a copy of the basic SceneNode with exactly the same dimensions as the Root!
    current_->parent(this);

    current_->layout().frame(false, frame_, *colour_, style_, thickness_, Colour("white"));
    current_->layout().resolve(false);
    items_.push_back(current_);
}

RootSceneNode::RootSceneNode() :
    absoluteWidth_(29.7),
    absoluteHeight_(20.),
    current_(0),
    scale_(1),
    newpage_(true),
    endpage_(false) {
    name_ = "root";
}

RootSceneNode::~RootSceneNode() {
    delete layout_;
}

/*!
 Class information are given to the output-stream.
*/
void RootSceneNode::print(ostream& out) const {
    out << "RootSceneNode[";
    out << "]";
}

MvRootSceneNode::MvRootSceneNode() {
    setPage(new MvRootScenePage());
    delete manager_;
    manager_ = new LayoutManager();
    // Metview is only using positional layout!
}

void MvRootSceneNode::getReady() {
    absoluteWidth_  = FortranRootSceneNodeAttributes::width_;
    absoluteHeight_ = FortranRootSceneNodeAttributes::height_;

    layout_ = new RootLayout(absoluteWidth_, absoluteHeight_);
    layout_->name(name_);
    widthResolution_  = absoluteWidth_ * 40;
    heightResolution_ = absoluteHeight_ * 40;
}


MvRootSceneNode::~MvRootSceneNode() {}

/*!
 Class information are given to the output-stream.
*/
void MvRootSceneNode::print(ostream& out) const {
    out << "MvRootSceneNode[";
    FortranRootSceneNodeAttributes::print(out);
    out << "]";
}

FortranRootSceneNode::FortranRootSceneNode() {
    setPage(new RootScenePage(absoluteWidth_, absoluteHeight_));
}

void FortranRootSceneNode::getReady() {
    absoluteWidth_  = FortranRootSceneNodeAttributes::width_;
    absoluteHeight_ = FortranRootSceneNodeAttributes::height_;

    if (layout_)
        delete layout_;
    layout_ = new RootLayout(absoluteWidth_, absoluteHeight_);
    layout_->name(name_);

    current_->resize(absoluteWidth_, absoluteHeight_);


    widthResolution_  = absoluteWidth_ * 40;
    heightResolution_ = absoluteHeight_ * 40;
    mode(basic);
    // layout_->frame(false, frame_, *colour_, style_, thickness_);
}


FortranRootSceneNode::~FortranRootSceneNode() {}

/*!
 Class information are given to the output-stream.
*/
void FortranRootSceneNode::print(ostream& out) const {
    out << "FortranRootSceneNode[";
    FortranRootSceneNodeAttributes::print(out);
    out << "]";
}

XmlRootSceneNode::XmlRootSceneNode() {
    setPage(new RootScenePage());
    absoluteWidth_  = 20.;
    absoluteHeight_ = 29.7;
    ParameterManager::set(string("layout"), string("magml"));
    mode(basic);
}


XmlRootSceneNode::~XmlRootSceneNode() {}

/*!
 Class information are given to the output-stream.
*/
void XmlRootSceneNode::print(ostream& out) const {
    out << "RootSceneNode[";
    out << "]";
}

void XmlRootSceneNode::getReady() {
    absoluteWidth_  = XmlRootNodeAttributes::width_;
    absoluteHeight_ = XmlRootNodeAttributes::height_;

    if (absoluteWidth_ == -1 || absoluteWidth_ == -1) {
        PaperDimension* dimension = MagTranslator<string, PaperDimension>()(format_);
        dimension->setOrientation(orientation_);
        absoluteWidth_  = dimension->getWidth();
        absoluteHeight_ = dimension->getHeight();
    }
    if (layout_)
        delete layout_;
    layout_           = new RootLayout(absoluteWidth_, absoluteHeight_);
    widthResolution_  = absoluteWidth_ * 40;
    heightResolution_ = absoluteHeight_ * 40;
    layout_->name(name_);
}

WrepRootSceneNode::WrepRootSceneNode() {
    absoluteWidth_  = 20.;
    absoluteHeight_ = 29.7;
    ParameterManager::set(string("layout"), string("magml"));
    ParameterManager::set(string("legend_box_blanking"), string("on"));
    ParameterManager::set(string("legend"), string("on"));
    ParameterManager::set(string("legend_wrep"), string("on"));

    mode(basic);
}


WrepRootSceneNode::~WrepRootSceneNode() {}

LegacyRootSceneNode::LegacyRootSceneNode() {}
LegacyRootSceneNode::~LegacyRootSceneNode() {}

/*!
 Class information are given to the output-stream.
*/
void WrepRootSceneNode::print(ostream& out) const {
    out << "RootSceneNode[";
    out << "]";
}
void LegacyRootSceneNode::print(ostream& out) const {
    out << "LegacySceneNode[";
    out << "]";
}
void WrepRootSceneNode::absoluteRootWidth(double width) {
    if (width != absoluteWidth_) {
        // We need to calculate the new pixel Width!
        pixel_width_ = width * 800. / 20.;
    }
    scale_          = pixel_width_;
    absoluteWidth_  = width;
    absoluteHeight_ = (pixel_height_ / pixel_width_) * absoluteWidth_;
}
void LegacyRootSceneNode::absoluteRootWidth(double width) {
    if (width < absoluteWidth_) {
        // We need to calculate the new pixel Width!
        pixel_width_ = width * 20. / 800.;
    }
    scale_          = pixel_width_;
    absoluteWidth_  = width;
    absoluteHeight_ = (pixel_height_ / pixel_width_) * absoluteWidth_;
}
void WrepRootSceneNode::getReady() {
    // 300 px by inch!

    absoluteWidth_  = (20. / 800.) * pixel_width_;
    absoluteHeight_ = (20. / 800.) * pixel_height_;

    widthResolution_  = pixel_width_;
    heightResolution_ = pixel_height_;


    if (layout_)
        delete layout_;
    layout_           = new RootLayout(absoluteWidth_, absoluteHeight_);
    widthResolution_  = absoluteWidth_ * 800. / 20.;
    heightResolution_ = absoluteHeight_ * 800. / 20.;
    layout_->name(name_);
}

void LegacyRootSceneNode::getReady() {
    // 300 px by inch!

    absoluteWidth_  = pixel_width_;
    absoluteHeight_ = pixel_height_;


    widthResolution_  = pixel_width_;
    heightResolution_ = pixel_height_;
    scale_            = (absoluteWidth_ / 20.) * 800.;


    if (layout_)
        delete layout_;
    layout_           = new RootLayout(absoluteWidth_, absoluteHeight_);
    widthResolution_  = absoluteWidth_ * 800. / 20.;
    heightResolution_ = absoluteHeight_ * 800. / 20.;
    layout_->name(name_);
}
void RootSceneNode::execute() {
    Timer timer("execute", "preparation of the graphical tree");


    dispatch(*layout_);
}

BasicGraphicsObject* RootSceneNode::visualise() {
    Timer timer("execute", "preparation of the graphical tree");

    return current_->visualise();
}

void RootSceneNode::release() {
    return current_->release();
}
BasicGraphicsObject* RootScenePage::visualise() {
    root_->setPage(*layout_);


    dispatch(*layout_);
    return layout_;
}

void RootScenePage::release() {
    items_.clear();
    delete layout_;
    layout_ = new RootLayout(width_, height_);
}
BasicSceneNode* RootSceneNode::insert(BasicPositionalObject* node) {
    getReady();
    RootScenePage* last = current_;
    current_->insert(node);
    if (last != current_)
        newpage_ = true;
    return current_;
}

BasicSceneNode* RootScenePage::newNode(BasicPositionalObject* node) {
    ASSERT(root_);

    RootScenePage* page = newPage();
    page->manager_      = manager_->clone();

    root_->setPage(page);

    node->orphan();
    (*(page->manager_))(page, node);

    return page;
}


BasicSceneNode* RootSceneNode::clone() {
    current_->newNode(0);

    return current_;
}

BasicSceneNode* FortranRootSceneNode::clone() {
    current_->newNode(0);
    current_->layout().frame(false, frame_, *colour_, style_, thickness_, Colour("white"));
    current_->layout().frameIt();
    current_->layout().resolve(false);
    return current_;
}

RootScenePage::RootScenePage(double width, double height) : root_(0), width_(width), height_(height) {
    static int i = 0;
    ostringstream n;
    n << "rootpage" << i;
    name_ = n.str();
    i++;

    layout_ = new RootLayout(width, height);
    layout_->name(name_);
}
RootScenePage::RootScenePage() : root_(0), width_(0), height_(0) {
    static int i = 0;
    ostringstream n;
    n << "rootpage" << i;
    name_ = n.str();
    i++;

    layout_ = new Layout();
    layout_->name(name_);
}
RootScenePage::~RootScenePage() {}

MvRootScenePage::MvRootScenePage() {
    delete manager_;
    manager_ = new LayoutManager();
    // Metview is only using positional layout!
}

MvRootScenePage::~MvRootScenePage() {}

BasicGraphicsObject* RootSceneNode::root() {
    layout_->resolve(mode_ == paper);
    return layout_;
}
void RootScenePage::resize(double width, double height) {
    width_  = width;
    height_ = height;
    ASSERT(layout_);
    layout_->resize(width, height);
}

void MvRootScenePage::getReady() {
    static int i = 0;
    ostringstream n;
    n << "basic" << i;
    name_ = n.str();
    i++;
    MagLog::dev() << "new getReady-->" << name_ << endl;


    BasicLayout* basic = new BasicLayout();
    ;

    basic->parent(layout_);

    layout_ = basic;


    layout_->name(name_);
}
static SimpleObjectMaker<A4, PaperDimension> a4("a4");
static SimpleObjectMaker<A3, PaperDimension> a3("a3");
static SimpleObjectMaker<A5, PaperDimension> a5("a5");
static SimpleObjectMaker<A6, PaperDimension> a6("a6");
