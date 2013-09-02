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

/*! \file Node.cc
    \brief Implementation of the Template class Node.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 11-Feb-2004
    
    Changes:
    
*/

#include "Node.h"
#include "Legend.h"
#include "Title.h"
#include "Layout.h"
#include "PaperDimension.h"
#include "MagTranslator.h"
#include "MagicsManager.h"


using namespace magics;



class magics::FrameNode : public BaseSceneObject
{
public:
	FrameNode(const RootNode& root) : 
		width_(root.getWidth()),
		height_(root.getHeight()),
		x_(0),
		y_(0)
    { 
		layout_ = new Layout();
		layout_->set(this);
		layout_->setLevel(1);
		layout_->box();
        //layout_->needsNewPage();
		addChild(new LayoutTask(layout_));
	}
	~FrameNode() { }
	virtual void set(const map<string, string>&) {}
	void setFrame(FrameBase* frame) {  (*layout_)["superroot"]->setFrame(frame); }
	void attachFrameTask() { addChild(new FrameTask(*layout_, "superroot")); } 
	double width() const  { return width_; }
    double height() const { return height_; }   
    void needsNewPage() { layout_->needsNewPage(); } 
    
    double rootWidth() const  { return width_; }
    double rootHeight() const { return height_; }
    
    double x() const      { return x_; }    
    double y() const      { return y_; }
    
    virtual double percentWidth() const  { return 100; }
    virtual double percentX() const      { return 0; }
    virtual double percentHeight() const { return 100; }
    virtual double percentY() const      { return 0; }
    Layout& getLayout()  { return *layout_; }
    
 
    
   
protected :
	double width_;
	double height_;
	double x_;
	double y_;
	Layout* layout_;
	
	
};

RootNode::RootNode() : frameNode_(0), width_(0), height_(0), frame_(0), metaData_(0), needsNewPage_(false) 
{	
}

RootNode::~RootNode()
{ 
	
}
    


void RootNode::set(const map<string, string>& params)
{
	SuperPageAttributes attributes;
	attributes.set(params);
	set(attributes);
}

void RootNode::set(const XmlNode& params)
{
    static bool needsNewPage = false;
	XmlPageAttributes attributes;
	attributes.set(params);
	set(attributes);
    if (needsNewPage)  frameNode_->needsNewPage();
    else needsNewPage = true;
}

void RootNode::set(const SuperPageAttributes& params)
{
	width_ = params.getWidth();
	height_ = params.getHeight();
	
	frame_ = params.getFrame().clone();
	Colour* colour = new Colour(params.getColour());
	frame_->setColour(colour);
	frame_->setStyle( params.getStyle()); 
	frame_->setThickness( params.getThickness());
	delete colour;
	
}

void RootNode::set(const XmlPageAttributes& params)
{
    PaperDimension* dimension = MagTranslator<string, PaperDimension>()(params.getFormat());
    dimension->setOrientation(params.getOrientation());
	
	width_ = dimension->getWidth();
	height_ = dimension->getHeight();
	ParameterManager::set("SUPER_PAGE_X_LENGTH", width_);
	ParameterManager::set("SUPER_PAGE_Y_LENGTH", height_);
	
	
	
	//MagLog::dev()<< "dispatch[" << width_ << ", " << height_ << "]\n";
	MagicsManager::dispatchDimension(width_, height_);
	

	if ( !frameNode_ ) {
		frameNode_ = new FrameNode(*this);
    	(*frameNode_).setParent(this);
    	children_.push_back(frameNode_);
	}
	frame_ = params.getFrame().clone();
	metaData_ = params.getMeta().clone();

	frameNode_->setFrame(frame_);
    frameNode_->attachFrameTask();
   
    
    MagLog::debug() << " setting of the Root..." << *this << "\n";
	
	delete dimension;
		
}


BaseSceneObject* RootNode::newXmlNode() 
{
	XmlPageAttributes attributes;
	set(attributes);
	
    return frameNode_;
}
 

BaseSceneObject* RootNode::newFortranNode() 
{
	SuperPageAttributes attributes;
	set(attributes);
	frameNode_ = new FrameNode(*this);
	(*frameNode_).setParent(this);
	children_.push_back(frameNode_);
	frameNode_->setFrame(frame_);
    frameNode_->attachFrameTask();
    return frameNode_;
}

BaseSceneObject* RootNode::newMetviewNode(const SuperPageAttributes& attributes) 
{
	
	set(attributes);
	ParameterManager::set("SUPER_PAGE_X_LENGTH", width_);
	ParameterManager::set("SUPER_PAGE_Y_LENGTH", height_);
	
	frameNode_ = new FrameNode(*this);
	(*frameNode_).setParent(this);
	children_.push_back(frameNode_);
	frameNode_->setFrame(frame_);
    frameNode_->attachFrameTask();
    return frameNode_;
}
  

void RootNode::addChild(BaseSceneObject* child) 
{
    if ( !frameNode_)  newXmlNode();
    frameNode_->addChild(child);
}

void RootNode::print(ostream& out)  const
{
	out << "Root[";
	BaseSceneObject::print(out);
	out << "]";
}

Layout& RootNode::getLayout() 
{ 
	assert(frameNode_);
	return frameNode_->getLayout();
}
	

void RootNode::clear()
{ 
	children_.clear(); 
	frameNode_ = 0;
	needsNewPage_ = false;
}

void RootNode::prepareGraphics() 
{
	BaseSceneObject::prepareGraphics();
	
	if (!metaData_) return;
	
	

	
	visit(*metaData_);
	
	
	metaData_->close();
	
}
