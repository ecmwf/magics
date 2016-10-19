/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Coastlines.cc 
    \brief Implementation of the Template class Coastlines.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 29-Jan-2004
    
    Changes:
    
*/

#include "Coastlines.h"
#include "Layer.h"

using namespace magics;
 
Coastlines::Coastlines() 
{
	setInfo("_datatype","Coastlines");
}

Coastlines::~Coastlines() { if ( layer_ ) delete layer_; }

/*!
 Class information are given to the output-stream.
*/		
void Coastlines::print(ostream& out)  const
{
	out << "Coastlines[";
	CoastlinesAttributes::print(out);
	out << "]";
}


#include "Transformation.h"

void Coastlines::visit(DrawingVisitor& parent)
{

	(*coastlines_)(parent);
	
	
	(*grid_)(parent);
	 

	(*label_).prepare(*grid_);
	
	(*label_)(parent);

}

void Coastlines::visit(LeftAxisVisitor& parent)
{
	(*label_).prepare(*grid_); 
	(*label_)(parent);
}
void Coastlines::visit(TextVisitor& )
{
	// Nothing to put in the title
}
void Coastlines::visit(LegendVisitor& legend)
{
	coastlines_->visit(legend);
}
void Coastlines::visit(RightAxisVisitor& parent)
{
	(*label_).prepare(*grid_); 
	(*label_)(parent);
}

void Coastlines::visit(TopAxisVisitor& parent)
{	
	(*label_).prepare(*grid_); 
	(*label_)(parent);
}

void Coastlines::visit(BottomAxisVisitor& parent)
{
	(*label_).prepare(*grid_); 
	(*label_)(parent);
}


void Coastlines::visit(PreviewVisitor& preview)
{	
	(*coastlines_)(preview);
}

void Coastlines::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors)
{
#ifdef MAG_NEXT
	// First we create the layer!
	// and push It to the parent layer! 
	if (layer.state() == geometry_changed ) {
		ASSERT (layer_);
		layer_->clean();
	}
	else 
#endif
    {
    // First we create the layer!
	// and push It to the parent layer! 
		layer_ = new NoDataLayer(this);
		
		layer_->icon(*this);
		
		layer.add(layer_);
	}
	for  (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
		layer_->set(*visitor);
		(*visitor)->visit(*this);
	}
	
}
void Coastlines::visit(MetaDataCollector& meta)
{

	MetviewIcon::visit(meta);
	coastlines_->visit(meta);
}

void Coastlines::visit(Transformation& transformation)
{
	label_->label(transformation);
}
