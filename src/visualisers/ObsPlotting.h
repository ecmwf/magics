/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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

#include "ObsPlottingAttributes.h"
#include "UserPoint.h"
#include "Visdef.h"

namespace magics {

class ObsPlotting : public Visdef, public ObsPlottingAttributes {
public:
    ObsPlotting();
    virtual ~ObsPlotting();
    virtual void set(const map<string, string>& map) { ObsPlottingAttributes::set(map); }

    virtual void operator()(Data&, BasicGraphicsObjectContainer&);
    void visit(MetaDataVisitor&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    // void filter(const CustomisedPointsList&, CustomisedPointsList&);


private:
    //! Copy constructor - No copy allowed
    ObsPlotting(const ObsPlotting&);
    //! Overloaded << operator to copy - No copy allowed
    ObsPlotting& operator=(const ObsPlotting&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ObsPlotting& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
