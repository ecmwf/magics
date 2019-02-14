/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BackgroundColour.h
    \brief Definition of Colour class.
    \author Meteorological Visualisation Section, ECMWF

    Started 2002

*/
#ifndef BackgroundColour_H
#define BackgroundColour_H


#include "Colour.h"
#include "Factory.h"
#include "MagTranslator.h"


namespace magics {

class BackgroundColour : public Colour {
public:
    BackgroundColour() {}
    BackgroundColour(const string& colour) : Colour(colour) {}
    BackgroundColour* clone() const { return new BackgroundColour(*this); }
};

template <>
class MagTranslator<string, BackgroundColour> {
public:
    BackgroundColour* operator()(const string& val) {
        if (Colour::valid(val))
            return new BackgroundColour(val);
        if (val == "background_colour")
            return new BackgroundColour();
        throw NoFactoryException(val);
    }

    BackgroundColour* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics

#endif
