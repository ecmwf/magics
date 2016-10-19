/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImportObject.h
    \brief Implementation of the Template class ImportObject.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 6-Apr-2005
    
    Changes:
    
*/

#include "ImportObject.h"

using namespace magics;


ImportObject::ImportObject()
{
}


ImportObject::~ImportObject() 
{
}

/*!
 Class information are given to the output-stream.
*/	

void ImportObject::print(ostream& out)  const
{
	out << "ImportObject[";
	out << "path=" << path_;
	out << ", format=" << format_;
	ImageProperties::print(out);
	out << "]";
}

bool ImportObject::reproject(BasicGraphicsObjectContainer& parent) const
{
	const Transformation& transformation = parent.transformation();
	ImportObject* object = new ImportObject();
	
	this->setObject(*object, transformation);
	parent.push_back(object);
	
	object->setFormat(format_);
	object->setPath(path_);
	return false;

}
