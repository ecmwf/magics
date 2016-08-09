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
