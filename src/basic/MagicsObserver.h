/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagicsObserver.h
    \brief Definition of the Template class MagicsObserver.

    Magics Team - ECMWF 2007

    Started: Thu 22-Nov-2007

    Changes:

*/

#ifndef MagicsObserver_H
#define MagicsObserver_H

#include "MagLog.h"
#include "magics.h"


namespace magics {


class MagicsCursorEvent;
class MagicsSwapBufferEvent;
class MagicsZoomEvent;
class MagicsAntialiasingEvent;
class MagicsAnimationEvent;
class MagicsMagnifierEvent;
class MagicsRestoreFbEvent;
class MagicsAnimationCurrentStepEvent;
class MagicsAnimationStepsEvent;
class MagicsLayerTreeEvent;
class MagicsLayerUpdateEvent;
class MagicsLayerSwapEvent;
class MagicsZoomUpdateEvent;
class MagicsRenderZoomPreviewEvent;

class MagicsObserver {
public:
    MagicsObserver() { MagLog::registerObserver(this); }
    virtual ~MagicsObserver() { MagLog::unregisterObserver(this); }


    virtual void warningMessage(const string& msg) { cout << msg; }   // default behaviour
    virtual void errorMessage(const string& msg) { cerr << msg; }     // default behaviour
    virtual void infoMessage(const string& msg) { cout << msg; }      // default behaviour
    virtual void progressMessage(const string& msg) { cout << msg; }  // default behaviour
    virtual void notify(MagicsCursorEvent&) {}
    virtual void notify(MagicsSwapBufferEvent&) {}
    virtual void notify(MagicsZoomEvent&) {}
    virtual void notify(MagicsAntialiasingEvent&) {}
    virtual void notify(MagicsMagnifierEvent&) {}
    virtual void notify(MagicsRestoreFbEvent&) {}
    virtual void notify(MagicsAnimationCurrentStepEvent&) {}
    virtual void notify(MagicsAnimationStepsEvent&) {}
    virtual void notify(MagicsLayerTreeEvent&) {}
    virtual void notify(MagicsLayerUpdateEvent&) {}
    virtual void notify(MagicsLayerSwapEvent&) {}
    virtual void notify(MagicsZoomUpdateEvent&) {}
    virtual void notify(MagicsRenderZoomPreviewEvent&) {}

    // virtual void areaCB(const string&, double ll_x, double ll_y, double ur_x, double ur_y) {}
    virtual void areaCB(const string&, double, double, double, double) {}
    virtual void inputCB(const string&, double, double) {}
    virtual void zoomCB(const string&, int) {}
    virtual void zoomControlCB(bool) {}
    virtual void animationControlCB(bool) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    MagicsObserver(const MagicsObserver&);
    //! Overloaded << operator to copy - No copy allowed
    MagicsObserver& operator=(const MagicsObserver&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MagicsObserver& p) {
        p.print(s);
        return s;
    }
};


}  // namespace magics
#endif
