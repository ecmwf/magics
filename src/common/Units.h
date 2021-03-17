/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Units.h
    \brief Definition of Units class.


    Changes:


*/
#ifndef Units_H
#define Units_H

#include <string>

namespace magics {


class Units {
public:
    static bool convert(const std::string& from, const std::string& to, double& scaling, double& offset);
    static void defaultScaling(double& scaling, double& offset, std::string& dataUnits, std::string& plotUnits);
    ;
};

}  // namespace magics
#endif
