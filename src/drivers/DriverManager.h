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
