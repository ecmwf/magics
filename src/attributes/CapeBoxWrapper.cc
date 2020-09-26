
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file CapeBoxAttributes.h
    \\brief Definition of CapeBox Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "CapeBoxWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


CapeBoxWrapper::CapeBoxWrapper(): capebox_(new CapeBox())

{

	
	
	
} 
CapeBoxWrapper::CapeBoxWrapper(CapeBox* capebox): capebox_(capebox)
{
	
	
} 

CapeBoxWrapper::~CapeBoxWrapper()
{
	
}

void CapeBoxWrapper::set(const MagRequest& request)
{
	
	

	if  (request.countValues("CAPE_BOX_BORDER_THICKNESS") ) {
		double box_border_thickness_value = request("CAPE_BOX_BORDER_THICKNESS");
		capebox_->box_border_thickness_ = box_border_thickness_value;
		}
	if  (request.countValues("CAPE_MARKER_INDEX") ) {
		int marker_index_value = request("CAPE_MARKER_INDEX");
		capebox_->marker_index_ = marker_index_value;
		}
	if  (request.countValues("CAPE_MARKER_HEIGHT") ) {
		double marker_height_value = request("CAPE_MARKER_HEIGHT");
		capebox_->marker_height_ = marker_height_value;
		}
	if  (request.countValues("CAPE_BOX_THICKNESS") ) {
		double box_thickness_value = request("CAPE_BOX_THICKNESS");
		capebox_->box_thickness_ = box_thickness_value;
		}
	if  (request.countValues("CAPE_BOX_WIDTH") ) {
		double box_width_value = request("CAPE_BOX_WIDTH");
		capebox_->box_width_ = box_width_value;
		}
	if  (request.countValues("CAPE_TEXT_FONT_SIZE") ) {
		double text_font_size_value = request("CAPE_TEXT_FONT_SIZE");
		capebox_->text_font_size_ = text_font_size_value;
		}
	
	if  (request.countValues("CAPE_CONTROL_COLOUR") ) {
		string control_colour_value = request("CAPE_CONTROL_COLOUR");
		capebox_->control_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(control_colour_value));
	}
		
	if  (request.countValues("CAPE_HRES_COLOUR") ) {
		string hres_colour_value = request("CAPE_HRES_COLOUR");
		capebox_->hres_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(hres_colour_value));
	}
		
	if  (request.countValues("CAPE_BOX_COLOUR") ) {
		string box_colour_value = request("CAPE_BOX_COLOUR");
		capebox_->box_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(box_colour_value));
	}
		
	if  (request.countValues("CAPE_BOX_BORDER_COLOUR") ) {
		string box_border_colour_value = request("CAPE_BOX_BORDER_COLOUR");
		capebox_->box_border_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(box_border_colour_value));
	}
		
	if  (request.countValues("CAPE_MARKER_COLOUR") ) {
		string marker_colour_value = request("CAPE_MARKER_COLOUR");
		capebox_->marker_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(marker_colour_value));
	}
		
	
	if  (request.countValues("CAPE_BOX_LINE_STYLE") ) {
		string box_style_value = request("CAPE_BOX_LINE_STYLE");
		capebox_->box_style_ = MagTranslator<string, LineStyle>()(box_style_value);
	}
		
	if  (request.countValues("CAPE_TEXT_FONT_COLOUR") ) {
		string text_font_colour_value = request("CAPE_TEXT_FONT_COLOUR");
		capebox_->text_font_colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(text_font_colour_value));
	}
		
	
}

void CapeBoxWrapper::print(ostream& out)  const
{
	out << "CapeBoxWrapper[]";
}


    









