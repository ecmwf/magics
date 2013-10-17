/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/*! \file Layout.cc
    \brief Implementation of the Template class Layout.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 30-Jan-2004
    
    Changes:
    
*/

#include "Layout.h"
#include "BaseDriver.h"

#include "Polyline.h"
#include "PaperPoint.h"
#include "SceneVisitor.h"

#include "Transformation.h"


using namespace magics;

const double axisHeight_ = 3;
const double axisWidth_ = 3;
const double legendHeight_ = 3;

#define CLEAR(a) if (a) delete a;

Layout::Layout()  :
	owner_(0),animationRules_(0), transformation_(0),
	width_(100), height_(100), x_(0), y_(0), display_(INLINE),
	xmin_(0), xmax_(100), ymin_(0), ymax_(100),
	zoomable_(false), navigable_(false), resizable_(false), resolve_(false)
{
}

Layout::~Layout()
{
	if ( owner_ )
		delete owner_;
}

void Layout::redisplay(const BaseDriver& driver) const
{
	if ( objects_.empty() && name_ != "drawing")
		return;
	MagLog::debug() <<  "Layout::redisplay-->" << *this << endl;
	driver.redisplay(*this);





}

bool Layout::reproject(BasicGraphicsObjectContainer& /*out*/) const
{
	//out.push_back(this));
	return true;
}

/*!
 Class information are given to the output-stream.
*/		
void Layout::print(ostream& out)  const
{
	out << "Layout[";
    out << name_;
    if (parent_) 
    	out <<", parent="<< parent_->name();
    out << ", x="<< x_;
    out << ", y="<< y_;
    out << ", width="<< width_;
    out << ", height="<< height_;
    out << ", xmin="<< xmin_;
    out << ", ymin="<< ymin_;
    out << ", xmax="<< xmax_;
    out << ", ymax="<< ymax_; 
    out << ", size="<< objects_.size();
	out << "]";
	for (vector<DriverInfo>::const_iterator info = driverInfos_.begin(); info != driverInfos_.end(); ++info) 
		out << *info << endl;
}


void Layout::absoluteWidth(double width) 
{
	width_ = 100*width / absoluteWidth();
}

void Layout::absoluteHeight(double height) 
{
	height_ = 100*height / absoluteHeight();
}

RootLayout::RootLayout(double width, double height) :
	absoluteWidth_(width), absoluteHeight_(height)
{	
}

RootLayout::~RootLayout()
{
}

PreviewLayout::~PreviewLayout()
{
}

PreviewLayout::PreviewLayout()
{
}

MagnifierLayout::~MagnifierLayout()
{
}

MagnifierLayout::MagnifierLayout()
{
}


LayoutFrame::LayoutFrame(): 
	thickness_(1), 
	style_(M_SOLID), 
	colour_("grey"), 
	blanking_(false), 
	visible_(false) 
{
}

LayoutFrame::~LayoutFrame()
{
}

Layout* Layout::execute(AnimationStep& /*step*/,  const Layout* /*visitor*/)
{
	return 0;
}

void LayoutFrame::blank(Layout& owner)
{

	
	if ( ! blanking_ ) return;
		// Create and push_back the frame!
		
	Polyline* frame = new Polyline();
	frame->setLineStyle(style_);
	frame->setThickness(thickness_);
	frame->setColour(visible_ ? colour_ : Colour("none"));
	frame->setFilled(true);      
	frame->setFillColour(Colour("white"));      
		
	FillShadingProperties* shading = new FillShadingProperties();          

	frame->setShading(shading);
		
	frame->push_back(PaperPoint(owner.minX(), owner.minY()));
	frame->push_back(PaperPoint(owner.minX(), owner.maxY()));
	frame->push_back(PaperPoint(owner.maxX(), owner.maxY()));
	frame->push_back(PaperPoint(owner.maxX(), owner.minY()));
	frame->push_back(PaperPoint(owner.minX(), owner.minY()));
	
	owner.push_back(frame);
}

void LayoutFrame::frame(Layout& owner)
{
	if ( ! visible_ ) return;
	// Create and push_back the frame!
	
	Polyline* frame = new Polyline();
	frame->setLineStyle(style_);
	frame->setThickness(thickness_);
	frame->setColour(colour_); 
	frame->setThickness(2);
		frame->setColour(Colour("charcoal"));
	double px = (owner.maxX() - owner.minX())*0.00;
	double py = (owner.maxY() - owner.minY())*0.00;
	
	
	frame->push_back(PaperPoint(owner.minX() +px , owner.minY() + py));
	frame->push_back(PaperPoint(owner.minX() +px , owner.maxY() - py));
	frame->push_back(PaperPoint(owner.maxX() - px, owner.maxY() - py));
	frame->push_back(PaperPoint(owner.maxX() - px , owner.minY() +py));
	frame->push_back(PaperPoint(owner.minX() + px, owner.minY() +py));
	
	owner.push_back(frame);
	
}

double Layout::absoluteX() const
{
//return parent_->absoluteX() + x_ * parent_->absoluteWidth() /100;
    return 0;
}

double Layout::absoluteY() const
{
	//return parent_->absoluteY() + y_ * parent_->absoluteHeight() /100;
	return 0;
}

double Layout::absoluteWidth() const
{
    assert(parent_);
    return width_ * parent_->absoluteWidth() /100;
}

double Layout::absoluteHeight() const
{
	assert(parent_);
	return height_ * parent_->absoluteHeight() /100;
}


void Layout::transformation(Transformation* transformation)
{
    	transformation_ = transformation;    
    	xmin_ = transformation_->getMinPCX();
    	xmax_ = transformation_->getMaxPCX();
    	ymin_ = transformation_->getMinPCY();
    	ymax_ = transformation_->getMaxPCY();
}

void Layout::redisplay(AnimationStep& step, const BaseDriver& driver)
{
	BasicGraphicsObject* plot = owner_->execute(step, *this);
	
	if (plot) 
		plot->redisplay(driver);
}


LayoutHelper::LayoutHelper() : 
		xmin_(INT_MAX), xmax_(-INT_MAX),  ymin_(INT_MAX), ymax_(-INT_MAX)
{
}

LayoutHelper::~LayoutHelper()
{
}

void LayoutHelper::add(LayoutVisitor* visitor)
{
	Layout* layout = visitor->mainLayout();
	if ( xmin_ > layout->x() ) 
		xmin_ = layout->x();
	if ( ymin_ > layout->y() ) 
		ymin_ = layout->y();
	if ( xmax_ < ( layout->x() +  layout->width() ) )
		xmax_ = layout->x()  +  layout->width();
	if ( ymax_ < ( layout->y() + layout->height() ) ) 
		ymax_ = layout->y() + layout->height();
	MagLog::dev() << "New Layout-->" << *this << endl;
}
	
void LayoutHelper::attachTop(LayoutVisitor* visitor)
{
	Layout* layout = visitor->mainLayout();
	layout->y(ymax_);
	layout->x(xmin_);
	layout->width(xmax_-xmin_);
	
}

void LayoutHelper::attachNoConstraintTop(LayoutVisitor* visitor)
{
	Layout* layout = visitor->mainLayout();
	layout->y(ymax_);
	layout->x(xmin_);
}

void LayoutHelper::attachLeft(LayoutVisitor* visitor)
{
	Layout* layout = visitor->mainLayout();
	layout->x(xmin_ - layout->width());
	layout->y(ymin_);
	layout->height(ymax_-ymin_);
}

void LayoutHelper::attachRight(LayoutVisitor* visitor)
{
	Layout* layout = visitor->mainLayout();
	layout->x(xmax_);
	layout->y(ymin_);
	layout->height(ymax_-ymin_);
}

void LayoutHelper::attachBottom(LayoutVisitor* visitor)
{
	Layout* layout = visitor->mainLayout();
	//if ( ymin_ - layout->height() > 0 )
		layout->y(ymin_ - layout->height());
	//else {
	//	layout->y(0);

		//layout->height(ymin_);
	//}
	layout->x(xmin_);
	layout->width(xmax_-xmin_);
}

void LayoutHelper::print(ostream& out)  const
{
	out << "LayoutHelper[";
	out << "xmin_=" << xmin_;
	out << "xmax_=" << xmax_;
	out << "ymin_=" << ymin_;
	out << "ymax_=" << ymax_;
	out << "]";
}

Layout*  Layout::clone() const
{
	Layout* layout = newLayout();
	
	layout->name_ = this->name_;
	layout->width_ = this->width_;
	layout->height_ = this->height_;
	layout->x_ = this->x_;	
	layout->y_ = this->y_;	
	
	layout->xmin_ = this->xmin_;		
	layout->xmax_ = this->xmax_;		
	layout->ymin_ = this->ymin_;	
	layout->ymax_ = this->ymax_;
	layout->transformation_ = this->transformation_;
	layout->parent_ = this->parent_;
	
	layout->id_ = this->id_;		
	layout->zoomable_ = this->zoomable_;
	layout->navigable_ = this->navigable_;
	layout->zoomLevels_ = this->zoomLevels_;		
	layout->zoomCurrentLevel_ = this->zoomCurrentLevel_;			
	layout->widthResolution_ = this->widthResolution_;
	layout->heightResolution_ = this->heightResolution_;
	layout->resizable_ = this->resizable_;				

	layout->frame(*this);
	
	return layout;
}

void RootLayout::redisplay(const BaseDriver& driver) const
{
	if ( resolve_) {
		intarray frames = driver.frames();
		unsigned int nb = frames.size();
		unsigned int current = 0;
		bool more = true;

		while (more) {

			unsigned int frame = (nb) ? frames[current]-1 : current;
			more = buildTree(*this, frame, driver);

			current++;
			if (current == nb)
				more = false;

			

		}

	}
	else
		driver.redisplay(*this);
}

void PreviewLayout::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
}

LegendLayout::LegendLayout()
{

}

LegendLayout::~LegendLayout()
{

}

void LegendLayout::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
}

void MagnifierLayout::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
}

void StartPage::redisplay(const BaseDriver& driver) const
{
	MagLog::dev() << "StartPage::redisplay-->" << *this << endl;
	
	driver.redisplay(*this);
}
void EndPage::redisplay(const BaseDriver& driver) const
{
	MagLog::dev() << "EndPage::redisplay-->" << *this << endl;
	
	driver.redisplay(*this);
}
HistoLayout::HistoLayout()
{
	
}

HistoLayout::~HistoLayout()
{
	
}

void HistoLayout::redisplay(const BaseDriver& driver) const
{

	driver.redisplay(*this);
}
SceneLayout::SceneLayout()
{

}

SceneLayout::~SceneLayout()
{

}

void SceneLayout::redisplay(const BaseDriver& driver) const
{
	if ( objects_.empty() )
		return;
	MagLog::debug() <<  "Layout::redisplay-->" << *this << endl;
    
	driver.redisplay(*this);
}
bool Layout::buildTree(const Layout& parent, unsigned int frame, const BaseDriver& driver) const
{
	bool more = false;

	for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object!= objects_.end(); ++object){
		if ( (*object)->buildTree(*this, frame, driver ) )
			more = true;
	}



	return more;
}

void Layout::release()
{
	if ( resolve_ ) {
		for (vector<BasicGraphicsObject*>::iterator object = objects_.begin(); object!= objects_.end(); ++object)
				(*object)->release();
	}
}

BasicLayout::BasicLayout()
{
}
BasicLayout::~BasicLayout()
{
}



bool BasicLayout::buildTree(const Layout& parent, unsigned int frame, const BaseDriver& driver) const
{
	bool more = false;


	StartPage* start = new StartPage();
	driver.redisplay(*start);

	for (vector<BasicGraphicsObject*>::const_iterator object = objects_.begin(); object!= objects_.end(); ++object){
		if ( (*object)->buildTree(*this, frame, driver ) )
			more = true;
	}

	EndPage* end = new EndPage();
	driver.redisplay(*end);
	return more;
}


vector<DriverInfo> Layout::driverInfos_;

