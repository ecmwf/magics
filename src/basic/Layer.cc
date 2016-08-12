/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Layer.h"
#include "BaseDriver.h"
#include "AnimationRules.h"

#include "SceneVisitor.h"
#include "TextVisitor.h"
#include "LegendVisitor.h"
#include "Symbol.h"
#include "BaseDriver.h"

#include "HistoVisitor.h"

//===================================
//
// Layer
//
//===================================

Layer::Layer() : 
visibility_(true),
zindex_(1),
transparency_(0),
state_(new_layer),
id_(""),
parent_(0),
object_(0)

{
}

Layer::Layer(BasicSceneObject* object) : 
			visibility_(true),
			zindex_(1),
			transparency_(0),
			state_(new_layer),
			id_(""),
			parent_(0),
			object_(object)
{
}

Layer::~Layer()
{
}

void Layer::execute(int , const BaseDriver&, const Layout&) const
{
	ASSERT(false);
}

void Layer::getReady(int) const
{
}
void Layer::release()
{
	if ( object_ ) object_->release();

	BasicGraphicsObjectContainer::clear();
}
void Layer::getReady() const
{
}

void  Layer::collect(ValuesCollector&  values)
{
	values.clear();
}

void Layer::newLayer(const BaseDriver& driver)
{
	driver.newLayer(*this); 
}

void Layer::closeLayer(const BaseDriver& driver)
{
	driver.closeLayer(*this); 
}

void Layer::redisplay(const BaseDriver& driver) const 
{
	driver.redisplay(*this); 
}

void Layer::print(ostream& out) const
{ 
	out << "layer[" << name_;
	string sep = "";
	for (vector<MetviewIcon>::const_iterator icon = icons_.begin(); icon != icons_.end(); ++icon) {
		out << ", [" << (*icon).iconName() << ", " << (*icon).iconClass()  << ", " << (*icon).iconId() << "]";
		sep = "]";
	}
	out << sep << "]"; 
}

string Layer::timeStamp(const string& fmt) const
{
	return from_.tostring(fmt);
}

string Layer::timeBegin(const string& fmt) const
{
	return from_.tostring(fmt);
}

string Layer::timeEnd(const string& fmt) const
{
	return to_.tostring(fmt);
}

static string iso_time("%Y-%m-%dT%H:%M:00Z");

string Layer::timeBegin() const 
{ 
	return timeBegin(iso_time); 
}

string Layer::timeEnd() const 
{ 
	return timeEnd(iso_time);  
}

string Layer::timeStamp() const
{
	return timeStamp(iso_time);
}

void Layer:: execute(const BaseDriver& ) const 
{
}

void Layer::collectText(vector<TextVisitor*>& texts, LegendVisitor* legend)
{
	
	if ( !object_) 
		return;

	for (vector<TextVisitor*>::iterator text = texts.begin(); text != texts.end(); ++text) {
		(*text)->visit(*object_);
		myTexts_[*text] = (*text)->texts();
	}
	if ( legend)
		legend->visit(*object_);
}

const string& Layer::metadata(const string& param)
{
	static string empty;
	map<string, string>::iterator data = metadata_.find(param);

	return (data != metadata_.end() ) ? data->second : empty;
}

void Layer::metadata(const string& param, const string& value)
{
	metadata_[param] = value;
}




//===================================
//
// SingleLayer
//
//===================================

SingleLayer::SingleLayer(StepLayer* parent, BasicSceneObject* object) : 
				Layer(object),
				objects_(0), parentLayer_(parent)
{
}

SingleLayer::~SingleLayer()
{
}

void SingleLayer::set(LayoutVisitor* visitor) const	
{
	visitor->newLayout();	
	objects_->push_back(visitor->layoutPtr());
}



void SingleLayer::print(ostream& ) const
{
}

void SingleLayer::redisplay(const BaseDriver& ) const
{
}

void SingleLayer::collect(MetaDataCollector& infos)
{
	if(object_)
		object_->visit(infos);
}

void SingleLayer::collect(ValuesCollector& values)
{
	object_->visit(values);
}

void SingleLayer::collect(DataIndexCollector& infos)
{
	if(object_)
		object_->visit(infos);
}

void SingleLayer::magnify(const BaseDriver& driver, float ,float )
{
	MagnifierCollector magnifier;
	for (vector<LayoutVisitor*>::iterator visitor = parentLayer_->firstVisitor(); 
			visitor != parentLayer_->endVisitor(); ++visitor)
	{
		(*visitor)->set(magnifier);
	}
	magnifier.setParent(parentLayer_);
	object_->visit(magnifier);
	magnifier.visit(driver);
}

void SingleLayer::execute(const BaseDriver& driver) const
{
//	if ( !parentLayer_->visibility() )
//		return;

	ASSERT(objects_);
	objects_->redisplay(driver);

}
void SingleLayer::release()
{
	Layer::release();
	objects_->clear();
}


void SingleLayer::update(const Layout& parent)
{
	ASSERT(objects_);
	objects_->width(parent.width());
	objects_->height(parent.height());
	objects_->x(parent.x());
	objects_->y(parent.y());
}

void SingleLayer::getReady() const
{
//	if ( !parentLayer_->visibility() )
//		return;
	if ( parentLayer_->parent()->state() == geometry_changed) {
		ASSERT(objects_);
		objects_->clear();
	}
	if ( !objects_ ) {
		objects_ = new Layout();
		objects_->name("singlelayer");
		objects_->parent(parentLayer_->parent());
		for (vector<LayoutVisitor*>::iterator visitor = parentLayer_->firstVisitor(); 
				visitor != parentLayer_->endVisitor(); ++visitor) {
			set(*visitor);
			(*visitor)->visit(*object_);
		}
	}
}


Layer* SingleLayer::baseLayer()
{ 
	return parentLayer_;
}


void SingleLayer::histogram(const BaseDriver& driver,const string& visdefName,const string& visdefClass)
{
	HistoVisitor histogram;
	MetviewIcon icon(visdefName,visdefClass);
	histogram.dataVisdefIcon(icon);
	object_->visit(histogram);
	histogram.redisplay(driver);
}



//===================================
//
// StepLayer
//
//===================================

StepLayer::StepLayer() 
{
}

StepLayer::~StepLayer() 
{
}

void StepLayer::redisplay(const BaseDriver& driver) const 
{  
	driver.redisplay(*this); 
}

void StepLayer::execute(int i, const BaseDriver& driver, const Layout& layout) const
{  
	if ( i < steps_.size() ) {
		steps_[i]->update(layout);
		steps_[i]->execute(driver);
	}
}

void StepLayer::getReady(int i) const
{  
	
	steps_[i]->getReady();
}

void StepLayer::newLayer(const BaseDriver& driver)
{
	driver.newLayer(*this); 
}

void StepLayer::closeLayer(const BaseDriver& driver)
{
	driver.closeLayer(*this); 
}

int StepLayer::size()
{
	return 1;
}

Layer* StepLayer::get(int i) 
{
	return steps_[i];
}

void StepLayer::addStep(BasicSceneObject* object)
{
	static int level = 100;
	SingleLayer* layer = new SingleLayer(this, object);
	layer->name(name_);
	layer->id(id_);
	static int mod = 0;
	static DateTime date = DateTime();

	layer->metadata("valid_date", string(date) );
	layer->metadata("level", tostring(level));
	level += 100;
	date = date + Second(6*3600*(mod%2));
	mod++;
	steps_.push_back(layer );
}

void StepLayer::addVisitor(LayoutVisitor* visitor)
{
	visitors_.push_back(visitor);
}

void StepLayer::print(ostream& out) const
{
	out << "StepLayer[";
	out << "]";
}

//===================================
//
// StaticLayer
//
//===================================

void StaticLayer::redisplay(const BaseDriver& driver) const 
{  
	MagLog::dev() << "Static::redisplay-->" << *this << endl;
	if ( updateText_ ) {
		updateText_ = false;

		const_cast<StaticLayer*>(this)->push_back(layer_);
	}

	driver.redisplay(*this);
}

void StaticLayer::execute(const BaseDriver& driver) const
{  

	
	redisplay(driver);
}

void StaticLayer::getReady() const
{  
}

void StaticLayer::collect(MetaDataCollector& infos)
{
	if(object_)
		object_->visit(infos);
}
	

void StaticLayer::collect(ValuesCollector& values)
{
	if(object_)
		object_->visit(values);
}

void StaticLayer::collect(DataIndexCollector& infos)
{
	if(object_)
		object_->visit(infos);
}

void StaticLayer::newLayer(const BaseDriver& driver)
{
	driver.newLayer(*this); 
}

void StaticLayer::closeLayer(const BaseDriver& driver)
{
	driver.closeLayer(*this); 
}

StaticLayer::StaticLayer() 
{
	layer_ = new Layout();
	layer_->name("staticlayer");
	updateText_ = true;
}
StaticLayer::StaticLayer(const Layout& layout)
{
	layer_ = layout.clone();
	updateText_ = true;
}
StaticLayer::StaticLayer(BasicSceneObject* object) : Layer(object) 
{
	layer_ = new Layout();
	layer_->name("staticlayer");
	updateText_ = true;;
}
void StaticLayer::update(const Layout& parent)
{
	layer_->name(parent.name());
	layer_->width(parent.width());
	layer_->height(parent.height());
	layer_->x(parent.x());
	layer_->y(parent.y());
}

void StaticLayer::clean()
{
	layer_->clear();
}

StaticLayer::~StaticLayer()
{
}

void StaticLayer::set(LayoutVisitor* visitor)
{
	//if ( layer_->isOrphan() )
	layer_->parent(parent_);

	visitor->newLayout();
	layer_->push_back(visitor->layoutPtr());
}

void StaticLayer::add(BasicGraphicsObject* object)
{
	if ( layer_->isOrphan() )
		layer_->parent(parent_);

	layer_->push_back(object);
}

void StaticLayer::print(ostream& out) const
{
	out << "StaticLayer[";
	out << "]";
}

void StaticLayer::histogram(const BaseDriver& driver,const string& visdefName, const string& visdefClass)
{
	HistoVisitor histogram;
	MetviewIcon icon(visdefName,visdefClass);
	histogram.dataVisdefIcon(icon);
	object_->visit(histogram);
	histogram.redisplay(driver);
}

//===================================
//
// NoDataLayer
//
//===================================

NoDataLayer::NoDataLayer(BasicSceneObject* object):StaticLayer(object) {}

NoDataLayer::~NoDataLayer() { }

void NoDataLayer::redisplay(const BaseDriver& driver) const 
{  

	if ( updateText_ ) {
		updateText_ = false;

		const_cast<NoDataLayer*>(this)->push_back(layer_);
	}

	driver.redisplay(*this);

}


//===================================
//
// TextLayer
//
//===================================

void TextLayer::getReady() const
{
	// Noothing to do!
}

void TextLayer::execute(const BaseDriver& driver) const
{
	// get the text for static later! 
	Layout* layout = new Layout();
	layout->parent(parent_);
	parent_->finishText(*layout);

	layout->redisplay(driver); 
}

void LegendLayer::getReady() const {}
void LegendLayer::execute(const BaseDriver&) const {}
void LegendLayer::execute(int, const BaseDriver&) const {}

void LegendLayer::getInfo(int, const BaseDriver& driver) const 
{
	// We assume hat "TextLayer::getInfo" has been called before!
	Layout parent;
	parent.parent(parent_);
	parent.name("Clone for legend");

	parent.width(parent_->layoutPtr()->width());
	parent.height(parent_->layoutPtr()->height());
	parent.x(parent_->layoutPtr()->x());
	parent.y(parent_->layoutPtr()->y());

	LegendVisitor* legend = this->parent()->legend();

	if ( legend ) {
		Layout* layout = new Layout();
		layout->parent(parent_);
		legend->finish(*layout);
		parent.push_back(layout);
	}
	parent.redisplay(driver);
	
}
	


void TextLayer::getInfo(int i, const BaseDriver&  driver) const
{
	// Get the information for a specifi step!
	parent_->executeInfo(i, driver); // here we get the information!

	Layout* parent = new Layout();
	parent->parent(parent_);
	parent->name("Clone of page");

	parent->width(parent_->layoutPtr()->width());
	parent->height(parent_->layoutPtr()->height());
	parent->x(parent_->layoutPtr()->x());
	parent->y(parent_->layoutPtr()->y());

	Layout* layout = new Layout();
	layout->parent(parent_);
	parent_->finishText(*layout);
	parent->push_back(layout);
	
	parent->redisplay(driver);
	delete parent;
}

void TextLayer::collectText(vector<TextVisitor*>& texts, LegendVisitor* legend)
{
	for (vector<TextVisitor*>::iterator text = texts.begin(); text != texts.end(); ++text) {
		(*text)->visit();
		myTexts_[*text] = (*text)->texts();
	}
}


//===================================
//
// SceneLayer
//
//===================================

SceneLayer::SceneLayer() : rules_(0), currentIndex_(0),legend_(0), currentFrame_(0)
{
	layout_ = new Layout();
}

SceneLayer::~SceneLayer()
{
	for (vector<Layer*>::iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
		(*layer)->clear();
	}
}

void SceneLayer::text(TextVisitor* text) 
{ 
	textVisitors_.push_back(text); 
	textHandler_.icon(*text); 
}

void SceneLayer::legend(LegendVisitor* legend)
{ 
	legend_ = legend; 
	legendHandler_.icon(*legend);
}

bool SceneLayer::buildTree(const Layout& parent,  unsigned int frame, const BaseDriver& out) const
{
	if (frame >= numberOfSteps() ) return false;
	if ( !currentFrame_ ) {
		textHandler_.name("Titles");
		textHandler_.parent(const_cast<SceneLayer*>(this));
	}
	// we need to copy the attributes from the parent!

	layout_->name(parent.name());
	layout_->width(parent.width());
	layout_->height(parent.height());
	layout_->x(parent.x());
	layout_->y(parent.y());
	layout_->frame(parent);
	layout_->blankIt();
	out.redisplay(*layout_);

	getReady(frame);
	execute(frame, out);
	textHandler_.getInfo(frame, out);
	layout_->frameIt();
	out.redisplay(*layout_);
	return ( frame+1 < numberOfSteps() );
}


void SceneLayer::redisplay(const BaseDriver& driver) const
{
	intarray frames = driver.frames();
	unsigned int nb = frames.size();
	switch ( mode_) {
	case paper: {
		textHandler_.name("Titles");
		textHandler_.parent(const_cast<SceneLayer*>(this));
		legendHandler_.name("Legend");
		legendHandler_.parent(const_cast<SceneLayer*>(this));
		if ( nb == 0) {
			for ( int i = 0; i < numberOfSteps(); i++ )
			{
				getReady(i);
				execute(i, driver);
				textHandler_.getInfo(i, driver);
				legendHandler_.getInfo(i, driver);
			}
		}
		else {
			for (unsigned int n = 0; n < nb; n++) {
				int i = frames[n] -1 ;
				if ( i < 0 || i >= numberOfSteps() ) {
					MagLog::warning() << " Export : could not find page " << i+1 << endl;
					continue;
				}

				getReady(i);
				execute(i, driver);
				textHandler_.getInfo(i, driver);
				legendHandler_.getInfo(i, driver);
			}
		}
	}
	break;
	case basic:
		textHandler_.name("Text");
		textHandler_.parent(const_cast<SceneLayer*>(this));
		legendHandler_.name("Legend");
		legendHandler_.parent(const_cast<SceneLayer*>(this));
		if ( nb == 0) {
			for ( int i = 0; i < numberOfSteps(); i++ )
			{
				getReady(i);
				execute(i, driver);
				textHandler_.getInfo(i, driver);
				legendHandler_.getInfo(i, driver);
			}
		}
		else  {
			for ( unsigned int i = 0; i < nb; i++) {
				int f =  frames[i];
				getReady(f);
				execute(f, driver);
				textHandler_.getInfo(f, driver);
				legendHandler_.getInfo(f, driver);
			}
		}
		break;
	case interactif:
		visit(driver);
		textHandler_.name("Titles");
		textHandler_.parent(const_cast<SceneLayer*>(this));
		const_cast<SceneLayer*>(this)->add(&textHandler_);
		const_cast<SceneLayer*>(this)->add(&legendHandler_);
		// here we add the Layer dedicated to the text ! ...
		driver.redisplay(*this);
		break;
	}

}

void SceneLayer::print(ostream& out) const
{
	out << "SceneLayer[";
	out << "]";
}

void SceneLayer::add(Layer* layer)
{
	layers_.push_back(layer);
	layer->parent(this); 
	layer->zindex(currentIndex_);
	currentIndex_++;
}

int SceneLayer::numberOfSteps() const
{
	int size =  (rules_) ? rules_->size() : 0;
	return std::max(1, size);
}

vector<Layer*>& SceneLayer::prepare(int i) const
{
	int size =  (rules_) ? rules_->size() : 0;

	if ( i >= size ) {
		vector<Layer*>* empty = new vector<Layer*>();
		return *empty;
	}
	map<int, vector<Layer*> >::iterator step = steps_.find(i);
	if ( step != steps_.end() ) 
		return step->second;
	ASSERT(rules_); 
	steps_.insert(make_pair(i, vector<Layer*>()));
	if ( i >= (int)(*rules_).size() )
	{
		for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
			if ( (*layer)->get() )
				steps_[i].push_back( (*layer)->get() );
		}
		return steps_[i];
	}

	//std::sort(layers_.begin(), layers_.end());// For the zindex!

	AnimationStep* anim = (*rules_)[i];

	for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
		map<Layer*, int>::iterator l = anim->find(*layer);
		if ( l == anim->end() ) {
			if ( (*layer)->get() )
				steps_[i].push_back( (*layer)->get() );
		}
		else
			steps_[i].push_back( (*layer)->get(l->second));
	}
	return steps_[i];
} 

void  SceneLayer::execute(int i, const BaseDriver& driver) const 
{
	// Look for the step! 
	int size =  (rules_) ? rules_->size() : 0;

	if ( i >= size )
	{
		if ( layers_.empty() ) {
			StaticLayer* layer = new StaticLayer(*layout_);

			layers_.push_back(layer);
			layers_.back()->parent(const_cast<SceneLayer*>(this));
		}
		for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
			(*layer)->update(*layout_);
			(*layer)->execute(driver);

		}
		return;
	}

	AnimationStep* step = (*rules_)[i];

	for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
		(*layer)->update(*layout_);
		map<Layer*, int>::iterator l = step->find(*layer);
		if ( l == step->end() )
			(*layer)->execute(driver);
		else
			(*layer)->execute(l->second, driver, *layout_);
	}
}

void  SceneLayer::execute(Layer* stepLayer, int i, const BaseDriver& out) const
{
	// Look for the step! 
	int size =  (rules_) ? rules_->size() : 0;
	if ( i >= size ) {		
		// here we have to send the txt!
		stepLayer->getInfo(i, out);
		return;
	}
	// Make sure that we need to execute this later!
	bool found=false;
	for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer)
	{
		if(*layer == stepLayer)
		{
			found=true;
			break;
		}
	}

	if(!found)
		return;


	// Look for the step! 
	ASSERT(rules_);    
	if  ( rules_->size() > i ) {    	
		AnimationStep* step = (*rules_)[i];
		map<Layer*, int>::iterator l = step->find(stepLayer);
		if ( l != step->end() )
			stepLayer->execute(l->second, out, *layout_);
		else
			stepLayer->getInfo(i, out);
	}
	else {
		ASSERT(false);
		//stepLayer->execute(i, driver);
	}
}

Layer*  SceneLayer::findLayer(Layer* stepLayer,int i) const 
{
	bool found=false;
	for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer)
	{
		if(*layer == stepLayer)
		{
			found=true;
			break;
		}
	}

	if(!found)
		return 0;

	// Look for the step! 
	ASSERT(rules_); 

	//Temporary fix!!
	if(i >= rules_->size())
		return stepLayer;

	AnimationStep* step = (*rules_)[i];

	map<Layer*, int>::iterator l = step->find(stepLayer);
	if (l == step->end()) 
		return 0;
	else 
		return stepLayer->get(l->second);
}

vector<Layer*>::iterator SceneLayer::beginLayer() const
{
	//std::sort(layers_.begin(), layers_.end());// For the zindex!
	return layers_.begin();
}

vector<Layer*>::iterator SceneLayer::endLayer() const
{
	return layers_.end();
}

void SceneLayer::getReady(int i ) const 
{
	if ( legend_ )
		legend_->clear();
	// Look for the step!
	int size =  (rules_) ? rules_->size() : 0;
	if ( i >=  size )
		// Only Static Layers!
	{
		for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
			(*layer)->getReady();
		}
	}
	else {
		ASSERT(rules_);
		AnimationStep* step = (*rules_)[i];

		for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
			map<Layer*, int>::iterator l = step->find(*layer);
			if ( l != step->end() )
				(*layer)->getReady(l->second);
		}
	}
}

vector<Layer*>::iterator SceneLayer::beginLayer(int i ) const
{
	return prepare(i).begin();
}

vector<Layer*>::iterator SceneLayer::endLayer(int i ) const
{
	return prepare(i).end();
}

void SceneLayer::redisplayAll(const BaseDriver& driver) const
{
	for ( int i = 0; i <= numberOfSteps(); i++ )
	{
		getReady(i);
		execute(i, driver);
	}
}

void SceneLayer::finishText(Layout& layout)
{
	for (vector<TextVisitor*>::iterator text = textVisitors_.begin(); text != textVisitors_.end(); ++text) 
		(*text)->finish(layout);
}


void SceneLayer::executeInfo(int i, const BaseDriver&) const
{
	// Look for the step! 
	vector<Layer*> layers;
	int size =  (rules_) ? rules_->size() : 0;
	for (vector<TextVisitor*>::iterator text = textVisitors_.begin(); text != textVisitors_.end(); ++text) {
		// we reset the text entries...
		(*text)->start();
	}
	if ( legend_ )
		legend_->clear();
	// We collect the static infos...
	textHandler_.collectText(textVisitors_, legend_);
	layers.push_back(&textHandler_);
	// Then we have to ask all the layers to collect and update the texts entries !
	if ( i >= size )
	{
		// here we have only static layers!
		for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
			(*layer)->collectText(textVisitors_, legend_);
			layers.push_back(*layer);
		}

	}
	else {
		ASSERT(rules_);

		if ( rules_->size() > 0 ) {
			AnimationStep* step = (*rules_)[i];

			for (vector<Layer*>::const_iterator layer = layers_.begin(); layer != layers_.end(); ++layer) {
				map<Layer*, int>::iterator l = step->find(*layer);
				if ( l == step->end() ) {
					// this is a static layer!
					(*layer)->collectText(textVisitors_, legend_);
					layers.push_back(*layer);

				}
				else {
					(*layer)->get(l->second)-> collectText(textVisitors_, legend_);
					layers.push_back((*layer)->get(l->second));
				}
			}
		}
	}

	// All text has been collected now we build the final result
	for (vector<TextVisitor*>::iterator text = textVisitors_.begin(); text != textVisitors_.end(); ++text)  {
		// Each line ...
		vector<string> lines;
		(*text)->titles(lines);
		for (vector<string>::reverse_iterator line = lines.rbegin(); line != lines.rend(); ++line) {
			//for ecah layer!

			for (vector<Layer*>::iterator layer = layers.begin(); layer != layers.end(); ++layer) {

				vector<Text*>& texts = (*layer)->updateText(*text, *line);
				(*text)->update(texts);
				texts.clear(); // we have used it.. clear it!
			}
		}
	}
}


//==================================
//
// Misc
//
//==================================

void MagnifierCollector::visit(const BaseDriver& driver)
{
	ASSERT(layout_);

	Symbol* points = new Symbol();
	points->setSymbol("magics_3"); // A little dot
	points->setHeight(0.2); 
	points->setColour(Colour("red"));

	for ( iterator point = begin(); point != end(); ++point)
		points->push_back(*point);

	layout_->push_back(points);
	layout_->redisplay(driver);
}


void MetviewIcon::visit(Layer& layer)
{
	if ( !iconClass_.empty() && !iconName_.empty() )
	{
		layer.icon(*this);
	}

}

void MetviewIcon::visit(MetaDataCollector& collector)
{
	if(information_.empty())
		return;

	if(collector.empty())
	{
		collector.insert(information_.begin(),information_.end());
	}
	else
	{
		for (map<string, string>::iterator key = collector.begin(); key != collector.end(); ++key )
		{
			if(information_.find(key->first) != information_.end())
			{
				key->second = information_[key->first];
			}
		}
	}
}


void LevelDescription::update(const LevelDescription& current) const
{

	set_ = current.set_;
	index_ = current.index_;
}

bool LevelDescription::operator < (const LevelDescription& other) const // DEclaration function in Data.h
{


	if ( this->surface_ ) {
		if ( other.surface_) {
			return  this->index_ < other.index_;
		}
		else
			return false;
	}


	if ( this->level_ == other.level_ )
		if ( this->set_ == other.set_ )
			return  this->index_ < other.index_;

	return  this->level_ > other.level_;
}


void DateDescription::update(const DateDescription& current) const
{

	set_ = current.set_;
	index_ = current.index_;
}

bool DateDescription::operator < (const DateDescription& other) const // DEclaration function in Data.h
{
	if ( this->valid_ == other.valid_ )
		if ( this->set_ == other.set_ )
			return this->index_ < other.index_;

	return DateTime(this->valid_) <  DateTime(other.valid_);
}

LevelDescription::LevelDescription(): surface_(true)
{
}

LevelDescription::~LevelDescription()
{
}

DateDescription::~DateDescription()
{
}

LevelDescription& SingleLayer::dataLevel() const
{
	static LevelDescription level;
	object_->visit(level_);
	return level_;
}

DateDescription& SingleLayer::timeStamp() const
{
	object_->visit(stamp_);
	return stamp_;
}

ValuesCollectorPoint::~ValuesCollectorPoint()
{
	while (!empty()) {
		ValuesCollectorData* data = back();
		pop_back();
		//delete data;
	}
}

void ValuesCollectorData::print(ostream& out) const
{
	out << "ValuesCollectorData[";
	out << "x=" << x_;
	out << ", y=" << y_;
	out << ", value=" << value_;
	out << ", distance=" << distance_;
	out << "]";
}

void ValuesCollectorUVData::print(ostream& out) const
{
	out << "ValuesCollectorData[";
	out << "x=" << x_;
	out << ", y=" << y_;
	out << ", xComponent=" << xComponent_;
	out << ", yComponent=" << yComponent_;
	out << "]";
}

void ValuesCollectorSDData::print(ostream& out) const
{
	out << "ValuesCollectorData[";
	out << "x=" << x_;
	out << ", y=" << y_;
	out << ", speed=" << speed_;
	out << ", direction=" << direction_;
	out << "]";
}

void ValuesCollectorVisitor::visit(const ValuesCollectorData& data)
{
}

void ValuesCollectorVisitor::visit(const ValuesCollectorUVData& data)
{
}

void ValuesCollectorVisitor::visit(const ValuesCollectorSDData& data)
{
}

ValuesCollectorVisitor::ValuesCollectorVisitor()
{
}
