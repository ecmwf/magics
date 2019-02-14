/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HistoIndividualMode.h
    \brief Definition of the Template class HistoIndividualMode.

    Magics Team - ECMWF 2004

    Started: Tue 18-May-2004

    Changes:

*/

#ifndef HistoIndividualMode_H
#define HistoIndividualMode_H

#include "magics.h"

#include "HistoIndividualModeAttributes.h"
#include "HistoMode.h"

namespace magics {

class Box;

class HistoIndividualMode : public HistoMode, public HistoIndividualModeAttributes {
public:
    HistoIndividualMode();
    virtual ~HistoIndividualMode();

    void set(const map<string, string>& map) { HistoIndividualModeAttributes::set(map); }
    void set(const XmlNode& node) { HistoIndividualModeAttributes::set(node); }
    HistoMode* clone() const {
        HistoIndividualMode* object;
        object->copy(*this);
        return object;
    }

    virtual void count(double, double);
    virtual void setToFirst(Layout&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    Box* addButton(int var, int val, Layout&);

    map<double, int> map_;

private:
    //! Copy constructor - No copy allowed
    HistoIndividualMode(const HistoIndividualMode&);
    //! Overloaded << operator to copy - No copy allowed
    HistoIndividualMode& operator=(const HistoIndividualMode&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const HistoIndividualMode& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
