/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CustomisedPoint.h
    \brief Definition of the Template class CustomisedPoint.

    Magics Team - ECMWF 2005

    Started: Thu 19-May-2005

    Changes:

*/

#ifndef CustomisedPoint_H
#define CustomisedPoint_H

#include "AutoVector.h"
#include "MagDateTime.h"
#include "magics.h"

namespace magics {

class CustomisedPoint : public map<string, double> {
public:
    CustomisedPoint() : latitude_(0), longitude_(0), identifier_(""), missing_(false), tile_(false) {}
    CustomisedPoint(double lon, double lat, string ident) :
        latitude_(lat),
        longitude_(lon),
        identifier_(ident),
        missing_(false),
        tile_(false) {}
    virtual ~CustomisedPoint() {}

    double latitude() const { return latitude_; }
    void latitude(double latitude) { latitude_ = latitude; }

    double longitude() const { return longitude_; }
    void longitude(double longitude) { longitude_ = longitude; }

    const DateTime& reference() const { return reference_; }
    void reference(DateTime ref) { reference_ = ref; }

    const DateTime& base() const { return base_; }
    void base(DateTime base) { base_ = base; }

    const DateTime& valid() const { return valid_; }
    void valid(DateTime valid) { valid_ = valid; }

    double referenceStep() const { return valid_ - reference_; }
    double step() const { return valid_ - base_; }

    string identifier() const { return identifier_; }
    void identifier(const string& identifier) { identifier_ = identifier; }

    string type() const { return type_; }
    void type(const string& type) { type_ = type; }
    bool missing() const { return missing_; }
    void missing(bool missing) { missing_ = missing; }
    bool tile() const { return tile_; }
    void tile(bool tile) { tile_ = tile; }


    double distance(double lat, double lon) const {
        return sqrt((lat - latitude_) * (lat - latitude_) + (lon - longitude_) * (lon - longitude_));
    }
    void copyProperties(const CustomisedPoint& other) {
        for (const_iterator value = other.begin(); value != other.end(); ++value) {
            insert(make_pair(value->first, value->second));
        }
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const {
        out << "CustomisedPoint[";
        out << "latitude=" << latitude_;
        out << ", longitude=" << longitude_;
        out << ", identifier=" << identifier_;
        for (const_iterator value = begin(); value != end(); ++value)
            out << ", " << value->first << "=" << value->second;
        out << "]";
    }
    double latitude_;
    double longitude_;
    string identifier_;
    string type_;
    DateTime base_;
    DateTime valid_;
    DateTime reference_;
    bool missing_;
    bool tile_;

private:
    //! Copy constructor - No copy allowed
    CustomisedPoint(const CustomisedPoint&);
    //! Overloaded << operator to copy - No copy allowed
    CustomisedPoint& operator=(const CustomisedPoint&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const CustomisedPoint& p) {
        p.print(s);
        return s;
    }
};


typedef AutoVector<CustomisedPoint> CustomisedPointsList;

}  // namespace magics
#endif
