/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/* \file DriverManager.cc
   \brief Implementation of Drivermanager class.
   \author Meteorological Visualisation Section, ECMWF

   Started: Jan 2004

*/

#include <DriverManager.h>
#include "BasicGraphicsObject.h"

using namespace magics;

DriverManager::DriverManager() 
{
}

DriverManager::~DriverManager() 
{
}

void DriverManager::print(ostream& out)  const
{
	out << "DriverManager";
}

void DriverManager::dispatch(BasicGraphicsObject* object) const
{

	if ( !object ) return;
	for (const_iterator driver = begin(); driver != end(); ++driver)
		if ( !(*(*driver)).disable() ) (*object).redisplay(*(*driver));
}

void DriverManager::dispatch(BaseDriver::ModeFunction mode, const SelectionMode& properties) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver)
		if ( !(*(*driver)).disable() ) ((*driver)->*mode)(properties);
}

void DriverManager::dispatch(BaseDriver::ControlFunction mode, bool control) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver)
		if ( !(*(*driver)).disable() ) ((*driver)->*mode)(control);
}

void DriverManager::dispatch(void (BaseDriver::*mode)()) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver)
		if ( !(*(*driver)).disable() ) ((*driver)->*mode)();
}

void DriverManager::dispatch(void (MagicsEvent::*notify)(MagicsObserver&), MagicsEvent& event) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver)
		if ( !(*(*driver)).disable() ) (event.*notify)(*(*driver));
}

void DriverManager::dispatch(BaseDriver::InputEventFunction mode, MtInputEvent* event) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver)
		if ( !(*(*driver)).disable() ) ((*driver)->*mode)(event);
}

void DriverManager::openDrivers() const 
{
	for (const_iterator driver = begin(); driver != end(); ++driver) 
		if ( !(*(*driver)).disable() ) (*(*driver)).open();
}

void DriverManager::closeDrivers() const 
{
	const_iterator driver = begin();
	for (; driver != end(); ++driver) 
		if ( !(*(*driver)).disable() ) (*(*driver)).close();
}

/*
void DriverManager::clearDrivers() const 
{
	for (const_iterator driver = begin(); driver != end(); ++driver) 
		if ( !(*(*driver)).disable() ) (*(*driver)).clear(); 
}
*/

void DriverManager::setDriversWidth(double width) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver) 
		if ( !(*(*driver)).disable() ) (*(*driver)).setXDeviceLength(width); 
}

void DriverManager::setOutputWidth(double width) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver) 
		if ( !(*(*driver)).disable() ){
			//double width = (*(*driver)).getWidth(); 
			(*(*driver)).setWidth(width); 
		}
}

void DriverManager::setDriversHeight(double height) const
{
	for (const_iterator driver = begin(); driver != end(); ++driver) 
		if ( !(*(*driver)).disable() ) (*(*driver)).setYDeviceLength(height); 
}
