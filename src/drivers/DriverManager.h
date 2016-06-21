/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/* \file DriverManager.h
   \brief Definition of driver manager class.
   \author Meteorological Visualisation Section, ECMWF

   Started: Jan 2004
   
*/
#ifndef DriverManager_H
#define DriverManager_H

#include <magics.h>
#include <BaseDriver.h>
#include <MagicsEvent.h>

namespace magics {

class BasicGraphicsObject;

/* \class DriverManager
   \brief Class to manage the various drivers.
   
*/
class DriverManager : public vector<BaseDriver*>
{
public:
	DriverManager();
	virtual ~DriverManager();
	void refresh();

	void openDrivers() const;
	void closeDrivers() const;
	void clearDrivers() const;

	void setDriversWidth(double) const;
	void setDriversHeight(double) const;
	void setOutputWidth(double) const;
	void dispatch(BasicGraphicsObject*) const;
	void dispatch(BaseDriver::ModeFunction, const SelectionMode&) const;
	void dispatch(BaseDriver::ControlFunction, bool) const;
	void dispatch(BaseDriver::InputEventFunction, MtInputEvent*) const;
	void dispatch(void (MagicsEvent::*)(MagicsObserver&), MagicsEvent&) const;
	void dispatch(void (BaseDriver::*)()) const;

protected:
	virtual void print(ostream&) const;

private:
// No copy allowed
	DriverManager(const DriverManager&);
	DriverManager& operator=(const DriverManager&);

// -- Friends
	friend ostream& operator<<(ostream& s,const DriverManager& p)
		{ p.print(s); return s; }
};
} // namespace magics
#endif
