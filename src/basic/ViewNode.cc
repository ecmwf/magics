/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ViewNode.cc
    \brief Implementation of the Template class ViewNode.
     
    Magics Team - ECMWF 2007
    
    Started: Tue 6-Mar-2007
    
    Changes:

*/

#include "ViewNode.h"
#include "Transformation.h"
#include "Dimension.h"
#include "Layout.h"
#include "Layer.h"

#include "SceneVisitor.h"
#include "LegendVisitor.h"
#include "TextVisitor.h"
#include "MetaData.h"

#include "HistoVisitor.h"

#include "AnimationRules.h"
#include <MagConfig.h>


using namespace magics;


ViewNode::ViewNode() : viewTransformation_(0), animation_("basic"), vaxis_(2.), haxis_(1.),  rules_(0), legend_(0), 
			needLegend_(false), drawing_background_colour_("white")
{
	static int i = 0;
	ostringstream n;
	n << "ViewNode" << i;
	name_ = n.str();
	i++;
	layout_ = new Layout();
	layout_->name(name_);

} 


ViewNode::~ViewNode() 
{
	for ( vector<LayoutVisitor*>::iterator  component = components_.begin(); component != components_.end(); ++component) {
		delete *component;
		*component = 0;
	}
	components_.clear();
}

void ViewNode::text(TextVisitor* text)
{
	texts_.push_back(text);
}

void ViewNode::legend(LegendVisitor* legend)
{
	legend_ = legend;
	needLegend_ = true;
}

void ViewNode::visit(MetaDataVisitor& metadata)
{
	double top = 0;
	double left = 0;
	double width = 200;
	double height = 100;

	drawing_->layout().getDriverInfo(left, top, width, height);

    // width and height are the dimensions of the layout!


	double imgwidth = width * 100 / (100-drawing_left_-drawing_right_);
	double imgheight = height * 100 / (100-drawing_top_-drawing_bottom_);


    metadata.add("output_width", tostring(rootWidthResolution()));
    metadata.add("output_height", tostring(rootHeightResolution()));
	viewTransformation_->visit(metadata, left, top, imgwidth, imgheight, width, height);

	BasicSceneObject::visit(metadata);
	for (vector<TextVisitor*>::iterator text = texts_.begin(); text != texts_.end(); ++text ) 
			(*text)->visit(metadata);
}

/*!
 Class information are given to the output-stream.
*/		
void ViewNode::print(ostream& out)  const
{
	out << "ViewNode[";
	out << "]";
}

void ViewNode::getReady()
{
	MagLog::dev() << " ViewNode::getReady() \n";
	BasicSceneObject::getReady();
}

void ViewNode::visit(PreviewVisitor& preview) 
{
	preview.minX(viewTransformation_->getMinPCX());
	preview.maxX(viewTransformation_->getMaxPCX());
	preview.minY(viewTransformation_->getMinPCY());
	preview.maxY(viewTransformation_->getMaxPCY());

	dispatch(preview);
}

void ViewNode::visit(HistoVisitor& histo) 
{
	dispatch(histo);
}




void ViewNode::prepareLayout(SceneLayer& tree)
{

	updateLayout();
	LayoutHelper helper;
	components_.clear();
	drawing_= new DrawingVisitor();
	frameHelper_= new FrameVisitor();
	const double width  = 100-drawing_left_-drawing_right_;
	const double height = 100-drawing_top_-drawing_bottom_;
	double vaxis  = 100/absoluteWidth() *vaxis_;
	if ( drawing_left_ - vaxis < 0 ) {
		vaxis = drawing_left_*0.80;
		MagLog::info() << "Automatically reduce the with of the vertical axis box to fit in the page" << endl;
	}

	drawing_->transformation(viewTransformation_);
	drawing_->y(drawing_bottom_);
	drawing_->x(drawing_left_);
	drawing_->height(height);
	drawing_->width(width);
	drawing_->id(id_);
	drawing_->widthResolution(widthResolution()*width/100);
	drawing_->heightResolution(heightResolution()*width/100);
	drawing_->zoomable(true);
	drawing_->zoomLevels(zoomLevels_);
	drawing_->zoomCurrentLevel(zoomCurrentLevel_);
	drawing_->frame(*layout_);
	drawing_->frameIt();
	drawing_->clippIt(layout_->clipp());


	frameHelper_->transformation(viewTransformation_);
	frameHelper_->y(drawing_bottom_);
	frameHelper_->x(drawing_left_);
	frameHelper_->height(height);
	frameHelper_->width(width);
	frameHelper_->widthResolution(widthResolution()*width/100);
	frameHelper_->heightResolution(heightResolution()*width/100);

	frameHelper_->frame(*layout_);
	frameHelper_->backgroundColour(drawing_background_colour_);
	
	frameHelper_->clippIt(false);

	//components_.push_back(frameHelper_); // first to draw the background if needed!
	components_.push_back(drawing_);
	
	helper.add(drawing_);

	// Then the axis!
	leftAxis_ = new LeftAxisVisitor(*drawing_);
	leftAxis_->width(vaxis);
	leftAxis_->frameIt();
	
	components_.push_back(leftAxis_);
	helper.attachLeft(leftAxis_);

	rightAxis_ = new RightAxisVisitor(*drawing_);
	rightAxis_->width(vaxis);
	rightAxis_->frameIt();
	
	helper.attachRight(rightAxis_);
	components_.push_back(rightAxis_);

	double topaxis  = (viewTransformation_->needTopAxis() ) ? haxis_ : 0.1;
	topaxis= 100/absoluteHeight()*topaxis;
	double bottomaxis= 100/absoluteHeight()*haxis_;

	topAxis_ = new TopAxisVisitor(*drawing_);
	topAxis_->height(topaxis);
	topAxis_->frameIt();
	
	helper.attachTop(topAxis_);
	components_.push_back(topAxis_);
	helper.add(topAxis_);


	bottomAxis_ = new BottomAxisVisitor(*drawing_);
	bottomAxis_->height(bottomaxis);
	bottomAxis_->frameIt();
	
	components_.push_back(bottomAxis_);
	helper.attachBottom(bottomAxis_);
	helper.add(leftAxis_);
	helper.add(rightAxis_);
		
	
	legend_ = tree.legend(legend_);
	if ( needLegend_ && legend_ )
	{
		if ( !legend_->positional() ) {
			if ( legend_->top() ) {
				legend_->height(5);
				helper.attachTop(legend_);
				legend_->x(drawing_left_);
				legend_->width(100-drawing_left_-drawing_right_);
			}
			else {
				legend_->width(7.5);
				helper.attachRight(legend_);
				legend_->y(drawing_bottom_);
				legend_->height(100-drawing_top_-drawing_bottom_);
			}
			helper.add(legend_);
		}
		((BasicSceneObject*)legend_)->parent((BasicSceneObject*)this);
	    legend_->getReady();
	    components_.push_back(legend_);
	}
	else 
		needLegend_ = false;
   
	

	for (vector<TextVisitor*>::iterator text = texts_.begin(); text != texts_.end(); ++text)
	{
		tree.text(*text);

		if ( !(*text)->positional() )
		{
			(*text)->height(15);
			helper.attachTop((*text));
			(*text)->x(drawing_left_);
			(*text)->width(100-drawing_left_-drawing_right_);
			(*text)->getReady();
		}
		else {
			((BasicSceneObject*)(*text))->parent((BasicSceneObject*)this);
			(*text)->getReady();
		}
	}
}



void ViewNode::copy(const ViewNode& other)
{
	viewTransformation_ = other.viewTransformation_;
	drawing_bottom_ = other.drawing_bottom_;
	drawing_top_ = other.drawing_top_;
	drawing_left_ = other.drawing_left_;
	drawing_right_ = other.drawing_right_;
	layout_->frame(*other.layout_);
}



void ViewNode::visit(SceneLayer& tree)
{
	MagLog::dev() << " ViewNode::visit(GraphicsList&) \n" << endl;
	viewTransformation_->init();
	BasicSceneObject::visit(*viewTransformation_); // to set up the automatic style for mapping!

	// drawing_->layout().blankIt();
	// First the info about the animation overlay! 
	// Basic instantiation of the Animation rules...
	
	
	

	if (!rules_) {
		rules_ = MagTranslator<string, AnimationRules>()(lowerCase(animation_));
		BasicSceneObject::visit(*rules_);
	}

	tree.rules(rules_);
	// Here we checking for the legend!
	needLegend_ = false;
	for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item)  {
			needLegend_ = (*item)->needLegend();
			if ( needLegend_ ) break;
	}
	bool blank = (drawing_background_colour_ != "none");
	push_front(new FrameBackgroundObject(blank, Colour(drawing_background_colour_)));
	
	//Here we have the steps! 	
	prepareLayout(tree);
	
	push_back(new FrameForegroundObject(frameIt_, frameColour_, frameStyle_, frameThickness_));
		
	
	if ( items_.empty() )
	{
		push_back(new EmptySceneObject() );
	}
	if ( needLegend_ ) {
		for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item)  {
			(*item)->getReady(*legend_);
		}
	}

	
	for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item)  {

		(*item)->visit(tree, components_);
	}
	
	//frameHelper_->frameIt();
	

	if( mode() == interactif )
	{
		PreviewVisitor* preview = new PreviewVisitor();
		tree.push_back(preview);

		preview->transformation(viewTransformation_);
		preview->height(100-drawing_top_-drawing_bottom_);
		preview->width(100-drawing_left_-drawing_right_);
		preview->visit(*this);		
	}
	if( mode() == basic || mode() == paper) // trial for batch release in Metview
	{
		// We do not need the data animore we clean!
		for ( vector<BasicSceneObject*>::iterator item = items_.begin(); item != items_.end(); ++item)  {
			(*item)->release();

		}
	}
}



XmlViewNode::XmlViewNode()
{
	//displayType_ = INLINE;
}

void XmlViewNode::updateLayout()
{
//	double width    = 100 - drawing_right_ - drawing_left_;
//	double height   = 100 - drawing_top_ - drawing_bottom_;
	
	drawing_bottom_ = ((drawing_bottom_* layout_->height())/100) + layout_->y();
	drawing_left_   = ((drawing_left_* layout_->width())/100 ) + layout_->x();
	double right = 100 - layout_->width() - layout_->x();
	double top = 100 - layout_->height() - layout_->y();
	drawing_right_  = ((drawing_right_* layout_->width())/100) + right ;
	drawing_top_    = ((drawing_top_* layout_->height())/100 ) + top;
	
	layout_->x(0);
	layout_->y(0);
	layout_->width(100);
	layout_->height(100);
}


XmlViewNode::~XmlViewNode()
{
}


void XmlViewNode::getReady()
{
	ASSERT (parent_);
	viewTransformation_ = XmlViewNodeAttributes::transformation_.get();

	Dimension bottom(bottom_, parent_->absoluteHeight(), 0);
	Dimension left(left_, parent_->absoluteWidth(), 0);
	Dimension width(XmlBasicNodeAttributes::width_, parent_->absoluteWidth(), 100);
	Dimension height(XmlBasicNodeAttributes::height_, parent_->absoluteHeight(), 100);

	Dimension mb(margin_bottom_, height.absolute(), 5);
	Dimension ml(margin_left_, width.absolute(), 7.5);
	Dimension mr(margin_right_, width.absolute(), 7.5);
	Dimension mt(margin_top_, height.absolute(), 10);

	drawing_bottom_ =mb.percent();
	drawing_left_ = ml.percent();
	

	double wab =  width.absolute()  - mr.absolute() - ml.absolute();
	double hab =  height.absolute()  - mt.absolute() - mb.absolute();
	vaxis_ = 1.;
	haxis_ = 0.5;
	double waa = wab;
	double haa = hab;
	MagLog::dev() << "after aspect ratio -->[" << waa << ", " << haa << "]" << endl; 

	
	double w2 = 100;
	double h2 =100;
	
	// Fitted can be: expand/tiling/crop/off

	if ( fitted_  == "expand" )
	{
		viewTransformation_->fill(waa, haa);
		absoluteRootWidth(waa);
		absoluteRootHeight(haa);
		viewTransformation_->aspectRatio(waa, haa);
		w2 = waa / width.absolute() *100;
		h2 = haa / height.absolute() *100;
	}
	else if ( fitted_  == "tiling" )
	{

		viewTransformation_->tile(waa, haa);

		absoluteRootWidth(waa);
		absoluteRootHeight(haa);


	}
	else if ( fitted_  == "crop" )
	{
		viewTransformation_->aspectRatio(waa, haa);
		absoluteRootWidth(waa); 
		absoluteRootHeight(haa);
	}

	else
	{
		viewTransformation_->aspectRatio(waa, haa);
				w2 = waa / width.absolute() *100;
				h2 = haa / height.absolute() *100;
		}
	drawing_top_ = 100 - drawing_bottom_ - h2;	
	drawing_right_ = 100 - drawing_left_- w2;
	

	layout_->x(left.percent());
	layout_->y(bottom.percent());
	layout_->width(width.percent());
	layout_->height(height.percent());
	frameIt_ = border_;
	frameColour_ = *border_colour_;
	frameStyle_ = border_style_;
	frameThickness_ = border_thickness_;
	drawing_background_colour_ = background_->name();
	layout_->display(display_);	 
	layout_->frame(true, border_, *border_colour_, border_style_, border_thickness_, *background_);

	BasicSceneObject::getReady();
}
void ViewNode::visit(MetaDataCollector&) {

}

void XmlViewNode::print(ostream&) const
{
}


FortranViewNode::FortranViewNode()
{
}


FortranViewNode::~FortranViewNode()
{
}


#define undef(x) x ==-1

class AdjustHelper
{
public:
	AdjustHelper(double top, double bottom, double height, double parent) : 
		top_(top), bottom_(bottom), height_(height), parent_(parent) {}
	~AdjustHelper() {}

	void operator()(double& top, double& bottom, double& height)
	{
		if ( undef(top))
		{
			if ( undef(bottom) )
			{
				if ( undef(height) ) {
					height = height_;
					bottom = bottom_;
					top = top_;
				}
				else { // we adjust to the top!
					top = top_;
					height = height / parent_ *100;
					double diff = height_ - height;
					if ( diff < 0 ) {
						// we try to reduce the top margin... 
						top += diff;
						if ( top < 0) {
							//we reduce the height..
							height = height_;
							top = top_;
						}

					}
					bottom = 100-top-height;
				}
			}
			else { // bottom is defined
				if ( undef(height) ) {
					bottom = bottom / parent_ *100;
					top = top_;
					height=100-bottom-top;
				}
				else { // we adjust to the top!
					bottom = bottom / parent_ *100;
					height=height / parent_ *100;
					top=100 -height-bottom;
				}			
			} 
		}
		else { // Top is defined 
			if ( undef(bottom) )
			{
				if ( undef(height) ) {
					top = top / parent_*100;
					bottom = bottom_;
					height = 100-top-bottom;
				}
				else {
					top = top / parent_ *100;
					height = height / parent_ *100;
					bottom = 100 -top-height;
				}
			}
			else { // bottom is defined
				if ( undef(height) ) {
					 top = top / parent_ *100;
					 bottom = bottom / parent_ *100;
					 height=100-bottom-top;
				}
				else {
					top = top / parent_ *100;
					bottom = bottom / parent_ *100;
					height=height / parent_ *100;
				}
			}
		}
	}

protected:
	double top_;
	double bottom_;
	double height_;
	double parent_;
};


void FortranViewNode::getReady()
{
	ASSERT (parent_);

	drawing_background_colour_ = background_->name();

	if ( predefined_ ) {
			MagDefLibrary library("projections");

			MagDef::Definition area;

			library.find(predefined_name_, area);
			
			viewTransformation_ = MagTranslator<string, Transformation>()(area.find("subpage_map_projection")->second);
			viewTransformation_->set(area);
			viewTransformation_->init();
	}
	else {
		viewTransformation_ = FortranViewNodeAttributes::transformation_.get();
	}
	viewTransformation_->setDefinition(json_);

//	MagLog::dev()<< *viewTransformation_ << endl;
	double left =  FortranViewNodeAttributes::left_;
	double right =  FortranViewNodeAttributes::right_;
	double top =  FortranViewNodeAttributes::top_;
	double bottom =  FortranViewNodeAttributes::bottom_;
	double width = FortranViewNodeAttributes::width_;	
	double height = FortranViewNodeAttributes::height_;
	animation_ = overlay_;
	
	AdjustHelper vertical(20, 5, 75,  parent_->absoluteHeight());
	AdjustHelper horizontal(5, 5, 90,  parent_->absoluteWidth());
	
	vertical(top, bottom, height);
	horizontal(left, right, width);
	
	double abswidth = width * absoluteWidth()  / 100;
	double absheight =  height * absoluteHeight()  / 100;
	double absx = left * absoluteWidth()  / 100;
	double absy =  bottom * absoluteHeight()  / 100;
	
	MagLog::dev() << "[" << abswidth << ", " << absheight << "]" << endl;
	MagLog::dev() << "[" << width << ", " << height << "]" << endl;

	viewTransformation_->aspectRatio(abswidth, absheight);
	MagLog::dev() << "after aspect ratio -->[" << abswidth << ", " << absheight << "]" << endl; 
	MagLog::dev() << "[" << abswidth << ", " << absheight << "]" << endl;
	MagLog::dev() << "[" << absoluteWidth() << ", " << absoluteWidth() << "]" << endl;

	width = abswidth / absoluteWidth() *100;
	height = absheight / absoluteHeight() *100;

	MagLog::dev() << "[" << width << ", " << height << "]" << endl;
	// This is for alignement.. need further test!
	double x = magCompare(horizontal_, "left") ?  left : 100 - left - width;
	double y = magCompare(vertical_, "bottom") ?  bottom : 100  - top - height;

	drawing_top_ = 100 - y - height;
	drawing_bottom_= y;
	drawing_right_ = 100 - x - width;
	drawing_left_ = x;



	vaxis_ = vertical_axis_with_;
	haxis_ = horizontal_axis_height_;

	
	
	ParameterManager::set("subpage_x_length_internal", abswidth);
	ParameterManager::set("subpage_y_length_internal", absheight);
	ParameterManager::set("subpage_x_position_internal", absx);
	ParameterManager::set("subpage_y_position_internal", absy);
	frameIt_ = frame_;
	frameColour_ = *frame_colour_;
	frameStyle_ = frame_line_style_;
	frameThickness_ = frame_thickness_;
	
	layout_->frame(true, frame_, *frame_colour_, frame_line_style_, frame_thickness_, *background_);
	layout_->clippIt(clipping_);

	BasicSceneObject::getReady();
}

void FortranViewNode::print(ostream& out) const
{
	out << "FortranViewNode[";
	BasicSceneObject::print(out);
	FortranViewNodeAttributes::print(out);
	out << "]";
}

BasicSceneNode* FortranViewNode::clone()
{
	FortranViewNode* node = new FortranViewNode();
	//node->copy(*this);
	return node;
}
static SimpleObjectMaker<NoOverlayAnimationRules, AnimationRules > never("never");
static SimpleObjectMaker<LevelAnimationRules, AnimationRules > by_level("by_level");
static SimpleObjectMaker<DateAnimationRules, AnimationRules > by_date("by_date");
static SimpleObjectMaker<AsIsAnimationRules, AnimationRules > always("always");
static SimpleObjectMaker<AsIsAnimationRules, AnimationRules > basic("basic");
static SimpleObjectMaker<NoOverlayAnimationRules, AnimationRules > none("none");
