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
#include "magplus.h"
#include "magics_ecmwf_log.h"
#include "MagLog.h"

#include "Timer.h"
#include "RootSceneNode.h"
#include "SceneNode.h"
#include "ViewNode.h"
#include "Coastlines.h"
#include "TextVisitor.h"
#include "LegendVisitor.h"
#include "VisualAction.h"
#include "GribDecoder.h"
#include "UserPointsDecoder.h"
#include "Contour.h"
#include "UserPoint.h"
#include "UserPoint.h"
#include "SymbolInput.h"
#include "SymbolPlotting.h"
#include "Wind.h"
#include "Axis.h"
#include "XYList.h"
#include "GraphPlotting.h"
#include "InputMatrix.h"
//#include "ImagePlotting.h"
#include "BoxPlotDecoder.h"
#include "BoxPlotVisualiser.h"
#include "SimplePolylineInput.h"
#include "SimplePolylineVisualiser.h"
#include "TitleTemplate.h"
#include "TaylorGrid.h"

#ifdef MAGICS_NETCDF
#include "NetcdfDecoder.h"
#endif

#ifdef MAGICS_BUFR
#include "ObsPlotting.h"
#include "ObsDecoder.h"
#endif


using namespace magics;

template <class T>
void MagDef::operator()(const string& name, const T& value)
{
	insert(make_pair(name, MagType(value)));
}


MagType& MagDef::operator()(const string& name) 
{
	static MagType empty;
	string param = name;
	BaseParameter* content = ParameterManager::getCopy(name);		
	vector<string>::const_iterator key = keys_.begin();
	while ( !content )	{
		if ( key == keys_.end() ) 
			return empty;
		param = *key + "_" + name;
		content = ParameterManager::getCopy(param);		
		++key;
	}
	insert(make_pair(param, MagType()));
	 (*this)[name].content_ = content;
	return (*this)[name];
}

void MagDef::print(ostream& out) const
{
	out << "[" << endl;
	for (const_iterator def = begin(); def != end(); ++def)
	{
		out << "\t" << def->first << " = " << def->second << endl;
		
	}
	out << "]" << endl;	
}

void  MagDef::set() const
{
	for (const_iterator def = begin(); def != end(); ++def)
	{
		if (def->second.content_) 
			ParameterManager::setLocal(def->second.content_);
	}
}

void  MagDef::reset() const
{
	for (const_iterator def = begin(); def != end(); ++def)
	{
		if ( def->second.content_) 
			ParameterManager::resetLocal(def->first);	
	}
}

void MagAction::action(Magics& magics) const
{
	set();
	build(magics);
	reset();
}

MagGrib::MagGrib()
{
	keys_.push_back("grib");
}

void MagGrib::print(ostream& out) const
{
	out << "GribDecoder";
	MagDef::print(out);
}

void MagGrib::build(Magics& magics) const
{
	magics.context(GEOGRAPHICAL);
	magics.node(new VisualAction<UserPoint>());;
	magics.data(new GribDecoder());
}

MagContour::MagContour()
{
	keys_.push_back("contour");
}

void MagContour::print(ostream& out) const
{
	out << "Contour";
	MagDef::print(out);
}


void MagContour::build(Magics& magics) const
{ 	
	switch (magics.context()) {
		case GEOGRAPHICAL: 
				magics.visdef(new Contour<UserPoint>() );
				break;
		case CARTESIAN:
				magics.visdef(new Contour<UserPoint>() );
				break;
		default :
				MagLog::warning() << "unknow context" << endl;
	}
}

MagCoastlines::MagCoastlines()
{
	keys_.push_back("map");
	keys_.push_back("map_coastline");
}
void MagCoastlines::print(ostream& out) const
{
	out << "Coastlines";
	MagDef::print(out);
}

void MagCoastlines::build(Magics& magics) const
{	
	Coastlines* coastlines = new Coastlines();
	magics.context(GEOGRAPHICAL);
	magics.pushTop(coastlines);
}


Magics::Magics(): drivers_(0), output_(0), node_(0)
{
	writeMagLog("fortran");
	if(getEnvVariable("MAGPLUS_QUIET").empty() )
	{
		MagLog::userInfo() << "------------------------------------------------------------------\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "			  "<< getMagicsVersionString() <<"\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << " Meteorological Applications Graphics Integrated Colour System\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "			    Developed By\n";	 
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "   The European Centre for Medium-Range Weather Forecasts\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "		      Copyright ECMWF "<<MAGICS_COPYRIGHT_PERIOD<<"\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "------------------------------------------------------------------\n";
	}
	actions_.push(&Magics::subpage);
	actions_.push(&Magics::page);
	actions_.push(&Magics::superpage);
	actions_.push(&Magics::drivers);
}


void Magics::drivers()
{
	if (!drivers_)  drivers_ = new DriverManager();
	if (!output_)   output_ = new OutputHandler();
	output_->set(*drivers_);
}

void Magics::subpage()
{
	FortranViewNode* node = new FortranViewNode();
	stack_.top()->push_back(node);
	stack_.push(node);
	//legend();

	while ( !axis_.empty() ) {
		node->push_back(axis_.top());
		axis_.pop();
	}
}


void Magics::page()
{
	while (stack_.top() != root_) 
		stack_.pop();
	FortranSceneNode* node = new FortranSceneNode();
	root_->insert(node);
	//stack_.top()->push_back(node);
	stack_.push(node);
}


void Magics::superpage()
{
	root_ = new FortranRootSceneNode();
	stack_.push(root_);
}

Magics::~Magics()
{
	actions();
	if ( root_ )
	{
		drivers_->setDriversWidth(root_->absoluteWidth());
		drivers_->setDriversHeight(root_->absoluteHeight());

		//legend();
		root_->getReady();
		root_->execute();
		{
			Timer timer("Drivers", "Render Graphical Tree");

			drivers_->openDrivers();
			/* later!
			for (GraphicsList::const_iterator object = root_->begin(); object != root_->end(); ++object)
				drivers_->dispatch(*object);
			*/	
			drivers_->closeDrivers();
		}
		delete root_;
		delete drivers_;
		delete output_;

		drivers_ = 0;
		root_ = 0;
		output_ = 0;
	}

	if(getEnvVariable("MAGPLUS_QUIET").empty() )
	{	
		MagLog::userInfo() << "------------------------------------------------------------------\n";
		MagLog::userInfo() << "    COMPLETED\n";
		MagLog::userInfo() << "\n";
		MagLog::userInfo() << "    Any problems or suggestions? Please contact us at\n";
		MagLog::userInfo() << "                magics@ecmwf.int\n";
		MagLog::userInfo() << "------------------------------------------------------------------\n";
	}
}


void Magics::execute()
{
	MagLog::dev() << "Magics open" << endl;
}


void Magics::add(const MagAction& action) 
{
	MagLog::dev() << "Magics plot-->" << action << endl;
	actions();
	action.action(*this);
}


void Magics::actions()
{
	while (!actions_.empty())
	{
		Action action = actions_.top();
		(this->*action)();
		actions_.pop();
	}
	MagLog::dev() << "actions-->" << size() << endl;
}

void Magics::pushTop(BasicSceneObject* object)
{ 
	assert(!stack_.empty());
	stack_.top()->push_back(object); 
}

template <class P>
void Magics::visdef(Visdef<P>* object)
{
	assert(node_);
	node_->visdef(object);
}

void Magics::node(BasicSceneObject* node)
{
	pushTop(node);
	node_ = node;
}

template <class P>
void Magics::data(Data<P>* object)
{
	assert(node_);
	node_->data(object);
}


template <class T> 
void Magics::operator()(const string& param, const T& value) 
{
	ParameterManager::set(param, value);
}

MagGlobalType& Magics::operator()(const string& param)
{
	insert(make_pair(param, MagGlobalType()));
	(*this)[param].param_ = param;
	return (*this)[param];
}
