
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file Akima474MethodAttributes.h
    \\brief Definition of Akima474Method Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "Akima474MethodWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


Akima474MethodWrapper::Akima474MethodWrapper(): akima474method_(new Akima474Method())

{

	
	
	ContourMethodWrapper::object(akima474method_);
	
	
} 
Akima474MethodWrapper::Akima474MethodWrapper(Akima474Method* akima474method): akima474method_(akima474method)
{
	
	
	ContourMethodWrapper::object(akima474method_);
	
} 

Akima474MethodWrapper::~Akima474MethodWrapper()
{
	
}

void Akima474MethodWrapper::set(const MagRequest& request)
{
	
	
	
	ContourMethodWrapper::set(request);
	

	if  (request.countValues("CONTOUR_AKIMA_X_RESOLUTION") ) {
		double resolutionX_value = request("CONTOUR_AKIMA_X_RESOLUTION");
		akima474method_->resolutionX_ = resolutionX_value;
		}
	if  (request.countValues("CONTOUR_AKIMA_Y_RESOLUTION") ) {
		double resolutionY_value = request("CONTOUR_AKIMA_Y_RESOLUTION");
		akima474method_->resolutionY_ = resolutionY_value;
		}
	
	
}

void Akima474MethodWrapper::print(ostream& out)  const
{
	out << "Akima474MethodWrapper[]";
}


    


