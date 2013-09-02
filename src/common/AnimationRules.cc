/*! \file FrameLoop.cc
    \brief Implementation of the Template class FrameLoop.
    
    Magics Team - ECMWF 2008
    
    Started: Fri 29-Aug-2008
    
    Changes:
    
*/


#include "AnimationRules.h"
#include "BaseDriver.h"
#include "Text.h"
#include "PaperPoint.h"
#include "Layer.h"
#include <limits>
#include "Data.h"

using namespace magics;


AnimationStep::AnimationStep(AnimationRules& rules) : rules_(rules),
xResolution_(std::numeric_limits<double>::max()), yResolution_(std::numeric_limits<double>::max())
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
	TagHandler::print(out); 
	for (map<Layer*, int>::const_iterator l = begin(); l != end(); ++l) 
		out << *(l->first) << "---> " << l->second << endl;
	out << "]";
}



void AnimationStep::rules(vector<string>& rules)
{ 
	rules_.rules(rules); 
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
	for ( vector<AnimationStep*>::const_iterator step = begin(); step != end(); ++step) 
		out << "step->" << **step << endl;
//	out << endl;
}

void AnimationRules::rules(vector<string>& ) const
{
}

void AsIsAnimationRules::rules(vector<string>& infos) const
{
	infos.push_back("<grib_info key='shortName'/>");
	infos.push_back("<grib_info key='valid-date'/>");
	infos.push_back("<grib_info key='level'/>");
}


AsIsAnimationRules::AsIsAnimationRules() : AnimationRules()
{
}

AsIsAnimationRules::~AsIsAnimationRules() 
{
}

void AnimationRules::add(StepLayer& )
{
	assert(false);
}

void AsIsAnimationRules::add(StepLayer& objects )
{
	int i = 0;
	for (vector<SingleLayer*>::iterator object = objects.firstStep(); object != objects.endStep(); ++object) {
		int x = size() -1;
		if ( i > x )  
			this->push_back(new AnimationStep(*this));
		(*this)[i]->insert(make_pair(&objects, i));		
		i++;
	}
}

NoOverlayAnimationRules::NoOverlayAnimationRules() : AnimationRules()
{
}

NoOverlayAnimationRules::~NoOverlayAnimationRules() 
{
}



void NoOverlayAnimationRules::add(StepLayer& objects )
{
    int i = 0;
	for (vector<SingleLayer*>::iterator object = objects.firstStep(); object != objects.endStep(); ++object) {	
		    AnimationStep* step = new AnimationStep(*this);
		    step->insert(make_pair(&objects, i));
		    push_back(step);	

			i++;
	}

}
void NoOverlayAnimationRules::rules(vector<string>& infos) const
{
	infos.push_back("<grib_info key='shortName'/>");
	infos.push_back("<grib_info key='valid-date'/>");
	infos.push_back("<grib_info key='level'/>");
}

/*!
 Class information are given to the output-stream.
*/		
void AsIsAnimationRules::print(ostream& out)  const
{
	out << "AsIsAnimationRules[" << this->size() << " steps";
	AnimationRules::print(out);
	out << "]";
}

/*!
 Class information are given to the output-stream.
*/		
void NoOverlayAnimationRules::print(ostream& out)  const
{
	out << "NoOverlayAnimationRules[" << this->size() << " steps";
	AnimationRules::print(out);
	out << "]";
}


DateAnimationRules::DateAnimationRules() : AnimationRules()
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
	out << "]";
}

void DateAnimationRules::rules(vector<string>& infos) const
{
	infos.push_back("<grib_info key='shortName'/>");
	infos.push_back("<grib_info key='valid-date'/>");
	infos.push_back("<grib_info key='level'/>");
}




void DateAnimationRules::add(StepLayer& objects )
{
    int i = 0;
	for (vector<SingleLayer*>::iterator object = objects.firstStep(); object != objects.endStep(); ++object) {

		    DateDescription& valid((*object)->timeStamp());

		    map<DateDescription, AnimationStep*>::iterator stepi = steps_.find(valid);
		    AnimationStep* step;
		    if (stepi ==  steps_.end() ) {
		    	step = new AnimationStep(*this);
		    	steps_.insert(make_pair(valid, step));
		    }
		    else {
		    	stepi->first.update(valid);
		    	step = stepi->second;
		    }

		    step->insert(make_pair(&objects, i));
			i++;
	}
	// We rebuild the vector...
	clear();
	for ( map<DateDescription, AnimationStep*>::iterator step = steps_.begin(); step != steps_.end(); ++ step) {
		push_back(step->second);
	}


}
LevelAnimationRules::LevelAnimationRules() : AnimationRules()
{
}

LevelAnimationRules::~LevelAnimationRules()
{
}

/*!
 Class information are given to the output-stream.
*/
void LevelAnimationRules::print(ostream& out)  const
{
	out << "DateAnimationRules[" << this->size() << " entries";
	out << "]";
}
void LevelAnimationRules::rules(vector<string>& infos) const
{
	infos.push_back("<grib_info key='shortName'/>");
	infos.push_back("<grib_info key='valid-date'/>");
	infos.push_back("<grib_info key='level'/>");
}

void LevelAnimationRules::add(StepLayer& objects )
{
	int i = 0;
	for (vector<SingleLayer*>::iterator object = objects.firstStep(); object != objects.endStep(); ++object) {

			LevelDescription& level((*object)->dataLevel());

			map<LevelDescription, AnimationStep*>::iterator stepi = steps_.find(level);
			AnimationStep* step;
			if (stepi ==  steps_.end() ) {
				step = new AnimationStep(*this);
				steps_.insert(make_pair(level, step));
			}
			else {
				stepi->first.update(level);
				step = stepi->second;
			}

			step->insert(make_pair(&objects, i));
			i++;
	}
	// We rebuild the vector...
	clear();
	for ( map<LevelDescription, AnimationStep*>::iterator step = steps_.begin(); step != steps_.end(); ++ step) {
		push_back(step->second);
	}
}



