/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CartesianTransformation.cc
    \brief Implementation of the Template class CartesianTransformation.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 19-Sep-2005
    
    Changes:
    
*/


#include "CartesianTransformation.h"
#include "Layout.h"
#include "MetaData.h"
#include "Polyline.h"
#include "ParameterSettings.h"
#include "MagJSon.h"

using namespace magics;


CartesianTransformation::CartesianTransformation() 
{
	dataReferenceX_ =  x_->reference();
	dataReferenceY_ =  y_->reference();
	referenceX_ =  x_->reference();
	referenceY_ =  y_->reference();
	topAxis_ = false;
	coordinateType_=XyType;
}

CartesianTransformation::~CartesianTransformation() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void CartesianTransformation::print(ostream& out)  const
{
	out << "CartesianTransformation[";
	CartesianTransformationAttributes::print(out);
	out << "]";
}


static void niceAdjust(double& min, double& max)
{
	float inc;
	int nb = 5;
	float step;
	float log, ws;

	while (nb < 20) {
		step = (max-min)/nb;
		log = log10(step);
		ws = pow(10., int(log));
		inc = ceil(step/ws)*ws;
		MagLog::dev() << "Automatic method ---> increment = " << inc << " ---> try base=" << inc/ws << endl;
		if ( inc/ws == 1 || inc/ws == 2 || inc/ws == 5 || inc/ws == 10 ) {
			MagLog::dev() << "Automatic method ---> increment " << inc << " OK! " << endl;
			break;
		}
		nb++;
		
	}
	
	float first = floor(min/inc) *inc;
	vector<float> values;
    
     
	for (float val = first;  val <= max +inc; val += inc) {
        	values.push_back(val);
	}
	
	if ( values.empty() ) {
		MagLog::warning() << " No input data --> return to default\n"; 
		min = 0;
		max = 100;
		return;
	}
	min = *std::min_element(values.begin(), values.end());
	max = *std::max_element(values.begin(), values.end());
}

void CartesianTransformation::adjustXAxis(Layout& layout) const
{
	MagLog::dev() << "CartesianTransformation::adjustXAxis()--->[" << dataMinX_ << ", " << dataMaxX_ << "]\n";
	if (  dataMinX_ == x_->min() && dataMaxX_ == x_->max() ) return;
	if ( dataReferenceX_ == "" ) niceAdjust(dataMinX_, dataMaxX_);
	MagLog::dev() << "CartesianTransformation::adjustXAxis()--->[" << dataMinX_ << ", " << dataMaxX_ << "]\n";
	
	if (dataMaxX_ - dataMinX_ < 4 ) dataMaxX_ = dataMinX_ + 4;// this needs to be done with more care later!
	// Should become a user option...
	const_cast<CartesianTransformation*>(this)->x_->minmax(dataMinX_, dataMaxX_);

	
	(*this)(layout);
}


void CartesianTransformation::adjustYAxis(Layout& layout) const
{
	MagLog::dev() << "CartesianTransformation::adjustYAxis()--->[" << dataMinY_ << ", " << dataMaxY_ << "]\n";
	if (  dataMinY_ == y_->min() && dataMaxY_ == y_->max() ) return;
	if ( dataReferenceY_ == "" ) niceAdjust(dataMinY_, dataMaxY_);
	MagLog::dev() << "CartesianTransformation::adjustYAxis()--->[" << dataMinY_ << ", " << dataMaxY_ << "]\n";
	if (dataMaxY_ - dataMinY_ < 4) dataMaxY_ = dataMinY_ + 4; // this needs to be done with more care later!
	// Should become a user option...
	const_cast<CartesianTransformation*>(this)->y_->minmax(dataMinY_, dataMaxY_);
	const_cast<CartesianTransformation*>(this)->referenceY_ = dataReferenceY_;

	(*this)(layout);
	
}


MAGICS_NO_EXPORT void CartesianTransformation::operator()(Layout& ) const
{
	/*
	if (layout["drawing_area"]) layout["drawing_area"]->setCoordinates(x_->minpc(), x_->maxpc(), y_->minpc(), y_->maxpc());
	if (layout["top_comment_box"]) layout["top_comment_box"]->setCoordinates(x_->minpc(), x_->maxpc(), -10, 10);
	if (layout["bottom_comment_box"]) layout["bottom_comment_box"]->setCoordinates(x_->minpc(), x_->maxpc(), -10, 10);
	if (layout["left_comment_box"]) layout["left_comment_box"]->setCoordinates(-10, 10, y_->minpc(), y_->maxpc());
	if (layout["right_comment_box"]) layout["right_comment_box"]->setCoordinates(-10, 10, y_->minpc(), y_->maxpc());
	if (layout["title"]) layout["title"]->setCoordinates(0, 100, 0, 100);
	if (layout["legend"]) layout["legend"]->setCoordinates(0, 100, 0, 100);
	*/
}
void CartesianTransformation::aspectRatio(double& width, double& height)
{
    init();
	double w = getAbsoluteMaxPCX() - getAbsoluteMinPCX();
	double h = getAbsoluteMaxPCY() - getAbsoluteMinPCY();
	return;
		if ( w/h >= width/height) {
			double nh = (h/w) * width;
			if ( nh <= height) {
				height = nh;
			}
			else {
				width = (w/h) * height;			
			}
		}
		else 
			width = (w/h) * height;
}

void CartesianTransformation::visit(MetaDataVisitor& visitor,
		double left, double top,
		double width, double height,
		double img_width, double img_height)
 {
         ostringstream java;
         double w = getMaxPCX() - getMinPCX();
         double h = getMaxPCY() - getMinPCY();
         java << "{";
         java << "\"name\" : \"cartesian\",";

         java << "\"top\" : \"" << top <<  "\",";
         java << "\"left\" : \"" << left <<  "\",";
         java << "\"width\" : \"" << width <<  "\",";
         java << "\"height\" : \"" << height <<  "\",";
         java << "\"img_width\" : \"" << img_width <<  "\",";
         java << "\"img_height\" : \"" << img_height <<  "\",";
         java << "\"pcxmin\" : \"" << getMinPCX() <<  "\",";
         java << "\"pcymin\" : \"" << getMinPCY() <<  "\",";
         java << "\"pcwidth\" : \"" << w <<  "\",";
         java << "\"pcheight\" : \"" << h <<  "\"";
         ostringstream def;
         CartesianTransformationAttributes::toxml(def);
         java << ",\"subpage_map_projection\":" << def.str() << endl;
         java << "}";
         visitor.add("projection", java.str());

 }

void CartesianTransformation::boxes() const
{
	if ( !PCEnveloppe_->empty() )
		return;

	PCEnveloppe_->box(PaperPoint(x_->minpc(), y_->minpc()), PaperPoint(x_->maxpc(), y_->maxpc()));
}

void CartesianTransformation::getNewDefinition(const UserPoint& ll, const UserPoint& ur, string& out) const
{
	map<string, string> def;
	def["subpage_map_projection"] = "cartesian";
	x_->getNewDefinition(ll, ur, def);
	y_->getNewDefinition(ll, ur, def);
	::toxml(out, def);

	out = "{" + out + "}";

	MagJSon helper;

	helper.interpret(out);



}

void CartesianTransformation::setDefinition(const string& json)
{
	if (json.empty())
		return;



	MagJSon helper;
	helper.interpret(json);

	XmlNode node = **helper.tree_.begin();

	node.name("cartesian");


	set(node);



}
void CartesianTransformation::reprojectComponents(double& x, double& y, pair<double, double>&) const
{
	fast_reproject(x, y);
}


