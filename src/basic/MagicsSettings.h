/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file MagicsSettings.h
    \brief Implementation of Global values of Magics.
    \author Meteorological Visualisation Section, ECMWF

    Started: JUme 2020
*/

#ifndef MagicsSettings_H
#define MagicsSettings_H

#include "MagicsSettingsAttributes.h"
#include "magics.h"
#include "magics_export.h"

namespace magics {

class MagicsSettings : public MagicsSettingsAttributes {
public:
    MAGICS_EXPORT static bool silent();
    MAGICS_EXPORT static void silent(bool s);
    MAGICS_EXPORT static bool compatibility();
    MAGICS_EXPORT static void compatibility(bool c);

    MAGICS_EXPORT static bool strict();
    MAGICS_EXPORT static void strict(bool c);

protected:
    MagicsSettings(){};
    ~MagicsSettings() override{};

    static MagicsSettings& instance();

    void print(ostream& s) const override { MagicsSettingsAttributes::print(s); }

private:
    //! Copy constructor - No copy allowed
    MagicsSettings(const MagicsSettings&);
    //! Overloaded << operator to copy - No copy allowed
    MagicsSettings& operator=(const MagicsSettings&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MagicsSettings& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
