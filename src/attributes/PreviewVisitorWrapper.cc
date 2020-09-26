
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file PreviewVisitorAttributes.h
    \\brief Definition of PreviewVisitor Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "PreviewVisitorWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


PreviewVisitorWrapper::PreviewVisitorWrapper(): previewvisitor_(new PreviewVisitor())

{

	
	
	NoPreviewVisitorWrapper::object(previewvisitor_);
	
	
} 
PreviewVisitorWrapper::PreviewVisitorWrapper(PreviewVisitor* previewvisitor): previewvisitor_(previewvisitor)
{
	
	
	NoPreviewVisitorWrapper::object(previewvisitor_);
	
} 

PreviewVisitorWrapper::~PreviewVisitorWrapper()
{
	
}

void PreviewVisitorWrapper::set(const MagRequest& request)
{
	
	
	
	NoPreviewVisitorWrapper::set(request);
	

	
	
}

void PreviewVisitorWrapper::print(ostream& out)  const
{
	out << "PreviewVisitorWrapper[]";
}


    


