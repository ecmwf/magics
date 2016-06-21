/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Frame.h
    \brief Implementation of the Template class Frame.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 29-Mar-2004
    
    Changes:
    
*/



#include "Frame.h"
#include "Polyline.h"
#include "PaperPoint.h"

using namespace magics;

Frame::Frame() 
{
}


Frame::~Frame() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void Frame::print(ostream& out)  const
{
	out << "Frame[";
	FrameAttributes::print(out);
	out << "]";
}

void NoFrame::print(ostream& out)  const
{
	out << "NoFrame[]";

}

void Frame::set(Polyline& frame)
{
	frame.setColour(*colour_);
    frame.setLineStyle(style_);
    frame.setThickness(thickness_);
}

void Frame::blank(Polyline& frame)
{
	
	//if (blanking_) {
		FillShadingProperties* shading = new FillShadingProperties();        
        shading->left_      = Colour("BACKGROUND");
        shading->right_     = *colour_;
        frame.setShading(shading);
        frame.setFilled(true);   
        frame.setColour(*colour_);
        frame.setFillColour(Colour("BACKGROUND"));
	//}
	
    
}

FrameBase* Frame::clone() const
{
	Frame* frame = new Frame();
	
	frame->copy(*this);
    return frame;
}


