/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SimplePolylineInput.h
    \brief Definition of the Template class SimplePolylineInput.

    Magics Team - ECMWF 2007

    Started: Mon 19-Mar-2007

    Changes:

*/

#ifndef SimplePolylineInput_H
#define SimplePolylineInput_H

#include "magics.h"

#include "Data.h"
#include "SimplePolylineInputAttributes.h"
#include "UserPoint.h"

namespace magics {

class SimplePolylineInput : public SimplePolylineInputAttributes, public Data, public PointsList {
public:
    SimplePolylineInput();
    virtual ~SimplePolylineInput();
    virtual void decode();
    void set(const map<string, string>& map) { SimplePolylineInputAttributes::set(map); }
    void set(const XmlNode& node) { SimplePolylineInputAttributes::set(node); }


    PointsHandler& points() {
        decode();
        pointsHandlers_.push_back(new PointsHandler(*this));
        return *(pointsHandlers_.back());
    }

    void customisedPoints(const Transformation& t, const std::set<string>& n, CustomisedPointsList& out, bool all) {}
    PointsHandler& points(const Transformation&, bool) { return points(); }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    SimplePolylineInput(const SimplePolylineInput&);
    //! Overloaded << operator to copy - No copy allowed
    SimplePolylineInput& operator=(const SimplePolylineInput&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const SimplePolylineInput& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
