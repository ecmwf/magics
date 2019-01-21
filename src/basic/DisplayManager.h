/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file DisplayManager.h
    \brief Definition of the Template class DisplayManager.

    Magics Team - ECMWF 2007

    Started: Fri 9-Mar-2007

    Changes:

*/

#ifndef DisplayManager_H
#define DisplayManager_H

#include "BasicGraphicsObject.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

namespace magics {

class BasicSceneObject;


class DisplayManager {
public:
    DisplayManager();
    virtual ~DisplayManager();


    void operator()(BasicSceneObject&, BasicGraphicsObjectContainer&);
    virtual void addInline(BasicSceneObject&, BasicGraphicsObjectContainer&);  // inline mode
    virtual void addBlock(BasicSceneObject&, BasicGraphicsObjectContainer&);   // block mode

    void bottomVertical(BasicSceneObject&, BasicGraphicsObjectContainer&);
    void bottomHorizontal(BasicSceneObject&, BasicGraphicsObjectContainer&);
    void topVertical(BasicSceneObject&, BasicGraphicsObjectContainer&);
    void topHorizontal(BasicSceneObject&, BasicGraphicsObjectContainer&);
    void nothing(BasicSceneObject&, BasicGraphicsObjectContainer&);

    void style(const string&, const string&, const string&);


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


    typedef void (DisplayManager::*DisplayFunction)(BasicSceneObject&, BasicGraphicsObjectContainer&);
    DisplayFunction style_;


private:
    //! Copy constructor - No copy allowed
    DisplayManager(const DisplayManager&);
    //! Overloaded << operator to copy - No copy allowed
    DisplayManager& operator=(const DisplayManager&);
    bool fortran_;

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const DisplayManager& p) {
        p.print(s);
        return s;
    }
    double x_;
    double y_;
};


}  // namespace magics
#endif
