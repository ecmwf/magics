
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file HiLoMarkerAttributes.h
    \\brief Definition of HiLoMarker Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "HiLoMarkerWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


HiLoMarkerWrapper::HiLoMarkerWrapper(): hilomarker_(new HiLoMarker())

{

	
	
	HiLoMarkerBaseWrapper::object(hilomarker_);
	
	
} 
HiLoMarkerWrapper::HiLoMarkerWrapper(HiLoMarker* hilomarker): hilomarker_(hilomarker)
{
	
	
	HiLoMarkerBaseWrapper::object(hilomarker_);
	
} 

HiLoMarkerWrapper::~HiLoMarkerWrapper()
{
	
}

void HiLoMarkerWrapper::set(const MagRequest& request)
{
	
	
	
	HiLoMarkerBaseWrapper::set(request);
	

	if  (request.countValues("CONTOUR_HILO_MARKER_HEIGHT") ) {
		double height_value = request("CONTOUR_HILO_MARKER_HEIGHT");
		hilomarker_->height_ = height_value;
		}
	if  (request.countValues("CONTOUR_HILO_MARKER_INDEX") ) {
		int index_value = request("CONTOUR_HILO_MARKER_INDEX");
		hilomarker_->index_ = index_value;
		}
	
	if  (request.countValues("CONTOUR_HILO_MARKER_COLOUR") ) {
		string colour_value = request("CONTOUR_HILO_MARKER_COLOUR");
		hilomarker_->colour_ = unique_ptr<Colour>(MagTranslator<string, Colour>()(colour_value));
	}
		
	
}

void HiLoMarkerWrapper::print(ostream& out)  const
{
	out << "HiLoMarkerWrapper[]";
}


    



