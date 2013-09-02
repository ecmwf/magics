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

/*! \file ObsPlotting.h
    \brief Definition of the Template class ObsPlotting.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 23-Mar-2005
    
    Changes:
    
*/

/*!
  \defgroup obs Plotting of observations
  
  Magics++ is able to plot observational data from ground, sea or space.
  Data can be read from BUFR and ODBs.
  
  \section obs_overview Plotting using WMO convention

  SYNOP, TEMP and METAR observations can be visulised with the so-called WMO convention
  
  \sa http://www.metoffice.gov.uk/corporate/library/factsheets/factsheet11.pdf

  \sa ../../share/magics/obs.xml
  
  \sa ../../share/magics/bufr_98.xml
  
  \sa ObsDecoder ObsWind ObsItem ObsItemBox ObsStationRing ObsTimePlot CustomisedPoint ComplexSymbol

*/

#ifndef ObsPlotting_H
#define ObsPlotting_H

#include "magics.h"

#include "Visdef.h"
#include "ObsPlottingAttributes.h"
#include "UserPoint.h"

namespace magics {

class ObsPlotting: public Visdef, public ObsPlottingAttributes {

public:
	ObsPlotting();
	virtual ~ObsPlotting();
	virtual void set(const map<string, string>& map) 
		{  ObsPlottingAttributes::set(map); }
	
    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(MetaDataVisitor&);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 //void filter(const CustomisedPointsList&, CustomisedPointsList&);


private:
    //! Copy constructor - No copy allowed
	ObsPlotting(const ObsPlotting&);
    //! Overloaded << operator to copy - No copy allowed
	ObsPlotting& operator=(const ObsPlotting&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ObsPlotting& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
