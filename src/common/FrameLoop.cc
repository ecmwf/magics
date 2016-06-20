/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file FrameLoop.cc
    \brief Implementation of the Template class FrameLoop.
    
    Magics Team - ECMWF 2008
    
    Started: Fri 29-Aug-2008
    
    Changes:
    
*/



#include "FrameLoop.h"
#include "BaseDriver.h"
#include "Text.h"
#include "PaperPoint.h"
using namespace magics;

FrameLoop::FrameLoop() 
{
	//concept_ = true;
}


FrameLoop::~FrameLoop() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void FrameLoop::print(ostream& out)  const
{
	out << "FrameLoop[" << size() << "entries";
	out << "]";
}

FrameEntry::FrameEntry() 
{
	static int index = 0;
	ostringstream name;
	
	name << "frame" << index++;
	name_ = name.str();
	label_ =  name.str();
	Text* text = new Text(LayerManager::layer(name_), "drawing_area");
	text->setText(name_);
	text->push_back(PaperPoint(0,0));
	//GraphicsList::push_back(text);
	
}


FrameEntry::~FrameEntry() 
{
}

void FrameEntry::visit(AnimationRules& rules) {
	MagLog::dev() << "Animation Rules to be applied -->" << rules <<endl;
	rules.callback(*this);
}
void  FrameEntry::tag(AnimationRules& rules)
{
	BasicSceneObject::visit(rules);
}

void FrameEntry::animate(AsIsAnimationRules& rules) 
{
	AnimationStep* step = rules.step(index_);
	step->push_back(this);	
}

void FrameEntry::execute(const BaseDriver& driver) 
{
// Prepare the graphics list! 
   if ( GraphicsList::empty() )
	BasicSceneObject::visit(*this);
	
   MagLog::dev() << "FrameEntry::execute()-->" <<*this << "[" << GraphicsList::size() << "]"<< endl;
   
   for (iterator object =begin(); object!=end();++object)
	   	(*object)->redisplay(driver);
}
/*!
 Class information are given to the output-stream.
*/		
void FrameEntry::print(ostream& out)  const
{
	out << "FrameEntry[" << name_ << ", "<< size() << "entries";
	out << "]\n";
	BasicSceneObject::print(out);
}

bool FrameLoop::reproject(const Transformation&, BaseGraphicsList& out) const
{
	out.push_back(const_cast<FrameLoop*>(this));
		return true;
}
void FrameLoop::redisplay(const BaseDriver& driver) const
{
	//static int i = 0;
	//if ( i == size() ) i = 0;
	//(*this)[i]->execute(driver);
	//i++;
	//for (const_iterator entry = this->begin(); entry != this->end(); ++entry) {
		//(*entry)->execute(driver);
	//}
	
	driver.redisplay(*this);
}


AnimationStep::AnimationStep() 
{
	
}


AnimationStep::~AnimationStep() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AnimationStep::print(ostream& out)  const
{
	out << "AnimationStep[" << size() << " entries";
	out << "]";
}

AnimationRules::AnimationRules() 
{
	
}


AnimationRules::~AnimationRules() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AnimationRules::print(ostream& out)  const
{
	out << "AnimationRules[" << size() << " steps";
	TagHandler::print(out);
	out << "]";
}
void AnimationRules::rules(vector<string>& infos) const
{
	
}


AsIsAnimationRules::AsIsAnimationRules() 
{
	
}


AsIsAnimationRules::~AsIsAnimationRules() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void AsIsAnimationRules::print(ostream& out)  const
{
	out << "AsIsAnimationRules[" << this->size() << " entries";
	TagHandler::print(out);
	out << "]";
}


AnimationStep* AsIsAnimationRules::step(int index)
{
	map<int, AnimationStep*>::iterator step = steps_.find(index);
	if ( step == steps_.end() )
		steps_.insert(make_pair(index, new AnimationStep()));
	 step = steps_.find(index);
	 return step->second;
	
}

void AsIsAnimationRules::rules(vector<string>& infos) const
{
	infos.push_back("<grib_info key='shortName'/>");
	infos.push_back("<grib_info key='stepRange'/>");
	infos.push_back("<grib_info key='level'/>");
}

void AsIsAnimationRules::getReady()
{
	MagLog::dev() << "AsIsAnimationRules --> get Ready!" << endl;
	
	for ( map<int, AnimationStep*>::iterator step = steps_.begin(); step != steps_.end(); ++step) {
		reset();
		for (AnimationStep::iterator frame = step->second->begin(); frame != step->second->end(); ++frame) {
			(*frame)->tag(*this);
		}
		push_back(step->second);
		MagLog::dev() << "tag-->" << step->first << " = " << *this << endl;
		string label = "param: " + get("grib", "shortName") + " (" + get("grib", "level") + ") at step:" +  get("grib", "stepRange");
		step->second->label(label);
	}
	
}

DateAnimationRules::DateAnimationRules() 
{
	
}


DateAnimationRules::~DateAnimationRules() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void DateAnimationRules::print(ostream& out)  const
{
	out << "DateAnimationRules[" << this->size() << " entries";
	TagHandler::print(out);
	out << "]";
}

