/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsGlobal.h
    \brief Implementation of Global values of Magics.
    \author Meteorological Visualisation Section, ECMWF

    Started: JUme 2020
*/

#ifndef MagicsGlobal_H
#define MagicsGlobal_H

#include "MagicsGlobalAttributes.h"
#include "magics.h"
#include "magics_export.h"

namespace magics {

class MagicsGlobal : public MagicsGlobalAttributes {
public:
    MAGICS_EXPORT static bool silent();
    MAGICS_EXPORT static void silent(bool s);
    MAGICS_EXPORT static bool compatibility();
    MAGICS_EXPORT static void compatibility(bool c);

    MAGICS_EXPORT static bool strict();
    MAGICS_EXPORT static void strict(bool c);

protected:
    MagicsGlobal(){};
    ~MagicsGlobal() override{};

    static MagicsGlobal& instance();

    void print(ostream& s) const override { MagicsGlobalAttributes::print(s); }

private:
    //! Copy constructor - No copy allowed
    MagicsGlobal(const MagicsGlobal&);
    //! Overloaded << operator to copy - No copy allowed
    MagicsGlobal& operator=(const MagicsGlobal&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MagicsGlobal& p) {
        p.print(s);
        return s;
    }
};



}  // namespace magics
#endif
