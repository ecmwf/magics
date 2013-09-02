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


