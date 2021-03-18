/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   JSONParser.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   Jun 2012

#include <fstream>

#include "JSONParser.h"
#include "Value.h"

namespace magics {


JSONParser::JSONParser(std::istream& in) : ObjectParser(in, false) {}

Value JSONParser::decodeFile(const std::string& path) {
    std::ifstream in(path.c_str());
    if (!in)
        throw CannotOpenFile(path);
    return JSONParser(in).parse();
}

Value JSONParser::decodeString(const std::string& str) {
    std::istringstream in(str);
    return JSONParser(in).parse();
}

//----------------------------------------------------------------------------------------------------------------------

Value JSONParser::parseValue() {
    return parseJSON();
}

std::string JSONParser::parserName() const {
    return "JSONParser";
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
