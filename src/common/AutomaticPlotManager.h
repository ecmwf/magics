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

/*! \file AutomaticPlotManager.h
    \brief Definition of the Template class AutomaticPlotManager.
    \author Meteorological Visualisation Section, ECMWF

    Started: Aug-2004

*/

#ifndef AutomaticPlotManager_H
#define AutomaticPlotManager_H

#include "magics.h"

#include "PlotManager.h"
#include "AutomaticPlotManagerAttributes.h"

namespace magics {

class AutomaticPlotManager: public PlotManager, public AutomaticPlotManagerAttributes  {

public:
	AutomaticPlotManager();
	virtual ~AutomaticPlotManager();
	virtual void set(const map<string, string>& map ) 
		{ AutomaticPlotManagerAttributes::set(map); }

	void page(MagicsManager&);
	void subpage(MagicsManager&);

	void addpage(MagicsManager&);
	void addRoot(MagicsManager& magics) 
		{ x_ = -1; y_ = -1; PlotManager::addRoot(magics); }
protected:
	enum PlotStart { BOTTOM, TOP };
	enum PlotDirection { VERTICAL, HORIZONTAL };
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	double x_;
	double y_;
	double pageX_;
	double pageY_;
	double pwidth_;
	double pheight_;
	double swidth_;
	double sheight_;

	PlotDirection direction();
	PlotStart start();

private:
	//! Copy constructor - No copy allowed
	AutomaticPlotManager(const AutomaticPlotManager&);
	//! Overloaded << operator to copy - No copy allowed
	AutomaticPlotManager& operator=(const AutomaticPlotManager&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AutomaticPlotManager& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
