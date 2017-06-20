/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file VisualAction.h
    \brief Implementation of the Template class ActionNode.
    
    Magics Team - ECMWF 2007
    
    Started: Tue 6-Mar-2007
    
    Changes:
    
*/ 

#include "VisualAction.h"
#include "LegendVisitor.h"
#include "Timer.h"
#include "TextVisitor.h"
#include "SceneVisitor.h"
#include "AnimationRules.h"
#include "Layer.h"
#include "MagnifierVisitor.h"
#include "HistoVisitor.h"

using namespace magics;


VisualAction::VisualAction() : data_(0), layer_(0)
{
}


VisualAction::~VisualAction()
{

	if (data_) delete data_;
	for (  vector<Visdef* >::iterator visdef = visdefs_.begin(); visdef != visdefs_.end(); ++visdef)
		{
			if ( *visdef )
				delete *visdef;
			*visdef = 0;
		}
	if (layer_) delete layer_;
}

/*!
 Class information are given to the output-stream.
*/	

void VisualAction::print(ostream& out)  const
{
	out << "ActionNode";
}


void VisualAction::getReady()
{
	MagLog::dev() << "ActionNode::getReady()\n";
}


void VisualAction::release()
{
	if ( data_ )
		data_->release();
}

void VisualAction::visit(DateDescription& timestamp) {
	if ( data_ )
		timestamp = data_->timeStamp();
}


void VisualAction::visit(LevelDescription& level) {
	if ( data_ )
		level = data_->level();
}




void VisualAction::visit(DrawingVisitor& drawing)
{
	if ( !data_ || ( data_ && !data_->valid() ) || visdefs_.empty() )
	{
		MagLog::warning() << " Check data or visual action!" << endl;
		return;
	}
	data_->getReady(drawing.transformation());

	for (  vector<Visdef* >::iterator visdef = visdefs_.begin(); visdef != visdefs_.end(); ++visdef)
	{
		Timer timer("plotting", "time spent in plotting");
		(**visdef).theme(theme());
		(**visdef)(*data_, drawing.layout()); // Now the visualObject ahs the responsability to reproject!
	}
}


void VisualAction::visit(HistoVisitor& drawing)
{
	if ( !visdefs_.empty() ) {
		// We only send it to the first action...
 
		drawing.basic(true);
		drawing.dataLayoutTransformation(&transformation());
		 
		for(  vector<Visdef* >::iterator visdef = visdefs_.begin(); visdef != visdefs_.end(); ++visdef)
		{
			if((*visdef)->iconName() == drawing.dataVisdefIcon().iconName() &&
			   (*visdef)->iconClass() == drawing.dataVisdefIcon().iconClass() && 
			   ! (*visdef)->iconClass().empty())
			{
				drawing.basic(false);
				(*visdef)->visit(*data_, drawing);
				return;	
			}
		}
		visdefs_.front()->visit(*data_, drawing);
	}
}



void VisualAction::visit(Transformation& transformation)
{
	if ( !data_ || !data_->valid() || visdefs_.empty() ) {
		MagLog::warning() << " No proper action defined!" << endl;
		return;
	}
	data_->visit(transformation);	
	// We need to make sure that the transformation is initialised properly!
	transformation.cleaninit();
	for ( vector<Visdef* >::iterator visdef = visdefs_.begin(); visdef != visdefs_.end(); ++visdef)
		(**visdef).visit(transformation, *data_);
}
 
 

void VisualAction::visit(LegendVisitor& legend)
{
	unsigned int entries = legend.size();
	bool needEmpty = false;
	legend.transformation(&transformation());
	for ( vector<Visdef* >::iterator visdef = visdefs_.begin(); visdef != visdefs_.end(); ++visdef) {
		if ( needEmpty ) 
			legend.add(new EmptyEntry() );
		entries = legend.size();

		(**visdef).visit(*data_, legend);

		needEmpty = ( entries != legend.size() );

	}
	if ( data_->valid() ) data_->visit(legend);
}


void VisualAction::visit(TextVisitor& title)
{
	if ( data_ &&  data_->valid() ) data_->visit(title);
}

void VisualAction::visit(TopAxisVisitor& top)
{
	for ( vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef) {
			(*visdef)->visit(top);
	}
}

void VisualAction::visit(MetaDataVisitor& infos)
{
	if ( data_ &&  data_->valid() ) data_->visit(infos);
	for ( vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef) {
		(*visdef)->visit(infos);
	}
}

void   VisualAction::getReady(const LegendVisitor& legend)
{
	for ( vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef) {
			(*visdef)->getReady(legend);
		}
}
void VisualAction::visit(MetaDataCollector& infos)
{
	infos.transformation(&transformation());
	if ( data_  && data_->valid() ) data_->visit(infos);
}


void VisualAction::visit(ValuesCollector& infos)
{
	infos.transformation(&transformation());
	if ( data_  && data_->valid() ) data_->visit(infos);
}

void VisualAction::visit(DataIndexCollector& infos)
{
	if ( data_ && data_->valid() ) data_->visit(infos);
}

void VisualAction::visit(MagnifierCollector& infos)
{
	infos.transformation(&transformation());
	if ( data_  && data_->valid() ) data_->visit(infos);
}


void VisualAction::visit(AnimationRules& rules)
{
	if ( data_  && data_->valid() ) data_->visit(rules);
}

const string VisualAction::name()
{
	return ( data_ ) ? data_->name() : "unknown";
}


const string VisualAction::id()
{
	return ( data_  ) ? data_->layerId() : "unknown";
}


VisualAnimation::VisualAnimation():loop_(0)
{
}


VisualAnimation::~VisualAnimation()
{
	if (loop_) delete loop_;
}


void VisualAnimation::prepare()
{
	if ( !this->empty() )
		return;
	
	layer_ = new StepLayer();
	layer_->id(loop_->id());
	
	loop_->visit(*layer_);
	
	for ( vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef)
	{    		
		(*visdef)->visit(*layer_);
	}
				
	loop_->setToFirst(); 
	while ( loop_->hasMore() )
	{
		MagLog::dev() << "New Frame" << endl; 

		VisualAction* action = new VisualAction();
		action->parent(this);
		action->data(loop_->current());

		for ( vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef) {
			action->visdef(*visdef);
		}

		layer_->addStep(action);

		loop_->next();
	}
}



void VisualAnimation::visit(MetaDataVisitor&)
{
}


void VisualAnimation::visit(AnimationRules& rules)
{
	prepare();
	for ( iterator entry = this->begin(); entry != this->end(); ++entry)
		(*entry)->visit(rules);
	rules.add(*this->layer_);
}



void VisualAnimation::visit(Transformation& transformation)
{
	ASSERT(loop_);
	loop_->visit(transformation);
} 


void VisualAnimation::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors)
{
	layer.add(layer_);
	for  (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
		layer.addVisitor(*visitor); 
		layer_->addVisitor(*visitor);  
	}
}

bool VisualAction::needLegend()
{
	for(vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef)
		{
			if ( (*visdef)->needLegend() )
				return true;
		}
	return false;
}

void VisualAction::visit(SceneLayer& layer, vector<LayoutVisitor*>& visitors)
{
	layer_ = new StaticLayer(this);

	
	layer_->icon(*data_);
	if ( data_ ) 
		data_->visit(*layer_);
 	layer.add(layer_);

	for  (vector<LayoutVisitor*>::iterator visitor = visitors.begin(); visitor != visitors.end(); ++visitor) {
		layer_->set(*visitor);
		(*visitor)->visit(*this);
	}

	for(vector<Visdef* >::iterator visdef = this->visdefs_.begin(); visdef != this->visdefs_.end(); ++visdef)
	{    
		(*visdef)->visit(*layer_);
	}
}
