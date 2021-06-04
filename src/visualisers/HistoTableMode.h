/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HistoTableMode.h
    \brief Definition of the Template class HistoTableMode.

    Magics Team - ECMWF 2004

    Started: Tue 18-May-2004

    Changes:

*/

#ifndef HistoTableMode_H
#define HistoTableMode_H

#include "magics.h"

#include "HistoMode.h"
#include "HistoTableModeAttributes.h"

namespace magics {

class HistoTableMode : public HistoMode, public HistoTableModeAttributes {
public:
    HistoTableMode();
    virtual ~HistoTableMode() override;
    void set(const map<string, string>& map) { HistoTableModeAttributes::set(map); }
    void set(const XmlNode& node) { HistoTableModeAttributes::set(node); }
    HistoMode* clone() const override {
        HistoTableMode* object;
        object->copy(*this);
        return object;
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const override;

private:
    //! Copy constructor - No copy allowed
    HistoTableMode(const HistoTableMode&);
    //! Overloaded << operator to copy - No copy allowed
    HistoTableMode& operator=(const HistoTableMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const HistoTableMode& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
