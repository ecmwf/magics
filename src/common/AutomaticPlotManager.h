/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AutomaticPlotManager.h
    \brief Definition of the Template class AutomaticPlotManager.
    \author Meteorological Visualisation Section, ECMWF

    Started: Aug-2004

*/

#ifndef AutomaticPlotManager_H
#define AutomaticPlotManager_H

#include "magics.h"

#include "AutomaticPlotManagerAttributes.h"
#include "PlotManager.h"

namespace magics {

class AutomaticPlotManager : public PlotManager, public AutomaticPlotManagerAttributes {
public:
    AutomaticPlotManager();
    virtual ~AutomaticPlotManager();
    virtual void set(const map<string, string>& map) { AutomaticPlotManagerAttributes::set(map); }

    void page(MagicsManager&);
    void subpage(MagicsManager&);

    void addpage(MagicsManager&);
    void addRoot(MagicsManager& magics) {
        x_ = -1;
        y_ = -1;
        PlotManager::addRoot(magics);
    }

protected:
    enum PlotStart
    {
        BOTTOM,
        TOP
    };
    enum PlotDirection
    {
        VERTICAL,
        HORIZONTAL
    };
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
    friend ostream& operator<<(ostream& s, const AutomaticPlotManager& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
