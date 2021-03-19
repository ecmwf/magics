/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @date Jun 2012

#include "MagParser.h"
#include "JSONParser.h"
#include "YAMLParser.h"

namespace magics {


Value MagParser::decodeFile(const std::string& path) {
    if (path.substr(path.size() - 5) == ".json") {
        return JSONParser::decodeFile(path);
    }

    return YAMLParser::decodeFile(path);
}

Value MagParser::decodeString(const std::string& str) {
    try {
        return JSONParser::decodeString(str);
    }
    catch (...) {
    }
    return YAMLParser::decodeString(str);
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
