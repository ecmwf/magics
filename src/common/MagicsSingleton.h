/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagicsSingleton.h

    \brief Definition of ...

    Magics Team - ECMWF 2006

    Started: Wed 27-Oct-2006

*/

#ifndef MagicsSingleton_H
#define MagicsSingleton_H

#include "magics.h"
#include "Data.h"
#include "UserPoint.h"
#include "Axis.h"
#include "Timer.h"
#include "XYList.h"
#include "GraphPlotting.h"
#include "EpsGraph.h"
#include "UserPointsDecoder.h"
#include "GribDecoder.h"
#include "OutputHandler.h"
#include "InputMatrix.h"

#include "MagicsSingletonAttributes.h"

namespace magics {

class MagicsSingleton : public MagicsSingletonAttributes
{
	MagicsSingleton() : magics_(0), 
		page_(false), 
		geoTask_(0), 
		xyTask_(0),
		timer_("Magics processing", MagLog::info()), 
		x_("x"), y_("y")
	{
		if (singleton_) return;
		singleton_ = this;
		magics_ = new MagicsManager();
		output_.set(*magics_);
		
	}    
	~MagicsSingleton() { MagLog::debug() << "Delete MagicsSingleton\n"; }
	static MagicsSingleton* singleton_;

public:
	static PlotManager& manager() 
	{
		if (!singleton_) new MagicsSingleton();
		return *(singleton_->layout_);
	}

	static void checkPage()
	{
		if (!singleton_) new MagicsSingleton();
		singleton_->layout_->check(*(singleton_->magics_));
	}

	static void execute() 
	{
		ASSERT(singleton_);
		MagLog::dev() << "Execute --->" << *(singleton_->magics_->root()) << "\n";
		singleton_->magics_->execute();
	}

	static VisualTask<UserPoint>* getGeoTask()
	{
		ASSERT(singleton_);
		return singleton_->geoTask();
	}

	static VisualTask<UserPoint>* getXYTask()
	{
		ASSERT(singleton_);
		return singleton_->xyTask();
	}

	static void close() 
	{
		if (!singleton_) new MagicsSingleton();
		MagLog::dev() << "Close --->" << *(singleton_->magics_->root()) << "\n";
		singleton_->magics_->execute();
		singleton_->magics_->close();
		delete singleton_;
		singleton_ = 0;
	}

	static void createTask(Data* data) 
	{
		if (!singleton_) new MagicsSingleton();
		checkPage();
		singleton_->geoTask_ = new VisualTask<UserPoint>();
		singleton_->layout_->add(singleton_->geoTask_);
		singleton_->geoTask_->set(data);
	}
    
	static void createTask(Data* data) 
	{
		if (!singleton_) new MagicsSingleton();
		checkPage();
		singleton_->xyTask_ = new VisualTask<UserPoint>();
		singleton_->layout_->add(singleton_->xyTask_);
		singleton_->xyTask_->set(data);
	}
	static void superpage()
	{
		if (!singleton_) new MagicsSingleton();
		singleton_->layout_->superpage(*(singleton_->magics_));
		singleton_->page_ = false;
//		singleton_->magics_->resetRoot();
	}

	static void page()
	{
		if (!singleton_) new MagicsSingleton();
		singleton_->layout_->page(*(singleton_->magics_));
		singleton_->page_ = true;
//		singleton_->magics_->resetRoot();
	}
    
	static void subpage()
	{
		if (!singleton_) new MagicsSingleton();
		singleton_->layout_->subpage(*(singleton_->magics_));
		singleton_->page_ = true;
//		singleton_->magics_->resetRoot();
	}
    
	static void add(BaseSceneObject* object) 
	{ 
		singleton_->layout_->add(object); 
	}

	static void addVisualiser(Visualiser<UserPoint>* visdef) 
	{
		if (!singleton_) new MagicsSingleton();
		if (!singleton_->xyTask_)
		{ 
			XYList* input = new XYList();        
			MagicsSingleton::createTask(input);
			MagLog::dev() << *input << "\n";
			//throw MagicsException("Plotting routine called prior any Data definition--> The call is ignored");
		}
		singleton_->xyTask_->set(visdef);
		singleton_->xyTask_ = 0;
	}

	static void addVisualiser(Visualiser<UserPoint>* visdef) 
	{
		if (!singleton_) new MagicsSingleton();
		if (!singleton_->geoTask_)
		{ 
			InputMatrix* input = new InputMatrix();     
			if (input->defined() ) {
				MagicsSingleton::createTask(input);
				MagLog::dev() << *input << "\n";
			}
			else {
				delete input;
				GribDecoder* grib = new GribDecoder();        
				MagicsSingleton::createTask(grib);
				MagLog::dev() << *grib << "\n";
			}
			
			
			//throw MagicsException("Plotting routine called prior any Data definition--> The call is ignored");
		}
		singleton_->geoTask_->set(visdef);
		singleton_->geoTask_ = 0;
	}
	VisualTask<UserPoint>* geoTask() { return geoTask_; }
	VisualTask<UserPoint>* xyTask() { return xyTask_; }
	static bool xIsDate() { 
		if (!singleton_) new MagicsSingleton(); 
		return (singleton_->x_ == "date"); 
	}
	static bool yIsDate() { 
		if (!singleton_) new MagicsSingleton(); 
		return (singleton_->y_ == "date"); 
	}
	static void xIsDate(bool date) { 
		if (!singleton_) new MagicsSingleton(); 
		singleton_->x_ = date  ? "date" : "x"; 
	}
	static void yIsDate(bool date) { 
		if (!singleton_) new MagicsSingleton(); 
		singleton_->y_ = date  ? "date" : "y"; 
	}
	static void setHorizontalAxis(HorizontalAxis* axis) { 
		if (!singleton_) new MagicsSingleton(); 
		singleton_->haxis_ = axis; 
	}
	static void setVerticalAxis(VerticalAxis* axis) { 
		if (!singleton_) new MagicsSingleton(); 
		singleton_->vaxis_ = axis; 
	}
	
	static bool isCartesianSystem() {
		
		return singleton_->haxis_ && singleton_->vaxis_; 
		
	}
	
	static void setCartesianSystem() {
		checkPage();
		if (singleton_->haxis_) {
			add(singleton_->haxis_);
			singleton_->haxis_ = 0;
		}
		if (singleton_->vaxis_) {
			add(singleton_->vaxis_);
			singleton_->vaxis_ = 0;
		}
		
	}
protected :
	MagicsManager*  magics_;
	bool 			page_;
	bool            superpage_;
	VisualTask<UserPoint>* geoTask_;
	VisualTask<UserPoint>* xyTask_;
	Timer timer_;
	string x_;
	string y_;
	HorizontalAxis* haxis_;
	VerticalAxis*   vaxis_;
	OutputHandler   output_;
};
} // namespace magics
#endif

