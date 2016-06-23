/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ViewNode.cc
    \brief Implementation of the Template class ViewNode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 6-Mar-2007
    
    Changes:
    
*/

#include "PreviewVisitor.h"
#include "Transformation.h"
#include "Layout.h"
#include "BaseDriver.h"

using namespace magics;



PreviewVisitor::PreviewVisitor()
{
name("preview");
}

PreviewVisitor::~PreviewVisitor()
{


}
void PreviewVisitor::print(ostream& s) const
{
	s << "PreviewVisitor[]";
}
void NoPreviewVisitor::print(ostream& s) const
{
	s << "NoPreviewVisitor[]";
}
void PreviewVisitor::visit(BasicGraphicsObjectContainer& tree)
{

	tree.push_back(this);
	// the Layout has been added to a Container, it will be delted automatically!
	
}
void PreviewVisitor::redisplay(const BaseDriver& driver) const { 
	MagLog::dev() << " I am a PreviewVisitor!" << endl;	
	driver.redisplay(*this);
}
void PreviewVisitor::visit(BasicSceneObject& object) { 
		object.visit(*this); 
}

