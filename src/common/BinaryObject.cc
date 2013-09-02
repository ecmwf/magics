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
