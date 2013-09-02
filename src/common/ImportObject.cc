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
