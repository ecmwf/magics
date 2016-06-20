/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BinaryObject.cc
    \brief Implementation of the Template class BinaryObject.
    
    Magics Team - ECMWF 2010
    
    Started: Fri 8-Jan-2010
    
    Changes:
    
*/

#include "BinaryObject.h"
#include "BaseDriver.h"
#include "SceneVisitor.h"
#include "Layer.h"

using namespace magics;

BinaryObject::BinaryObject() 
{
}

BinaryObject::~BinaryObject() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void BinaryObject::print(ostream& out)  const
{
	out << "BinaryObject[";
	BinaryObjectAttributes::print(out);
	out << "]";
}

void  BinaryObject::redisplay(const BaseDriver& driver) const
{
	driver.redisplay(*this);
}

void BinaryObject::visit(SceneLayer& layer)
{
	// First we create the layer!
	// and push It to the parent layer! 
	StaticLayer* bin = new StaticLayer(this);
	bin->name(path_);
	layer.add(bin);
	BinaryObject* binary = new BinaryObject();
	binary->copy(*this);
	bin->add(binary);
}
