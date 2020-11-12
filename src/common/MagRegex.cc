/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "MagRegex.h"
#include "MagException.h"


#include <regex>

//----------------------------------------------------------------------------------------------------------------------

namespace magics {

//----------------------------------------------------------------------------------------------------------------------

Regex::Regex(const std::string& s, bool shell) : str_(s) {
    // Log::debug() << "Regex " << str_ << std::endl;
    if (shell) {
        long len = s.length() * 3 + 1;
#ifdef MAGICS_ON_WINDOWS
        char buffer[1024];
#else
        char buffer[len];
#endif
        char* re = buffer;

        std::string::size_type i = 0;
        int j                    = 0;

        if (shell) {
            re[j++] = '^';
        }

        while (i < s.length()) {
            switch (s[i]) {
                case '?':
                    re[j++] = '.';
                    break;

                case '*':
                    re[j++] = '.';
                    re[j++] = '*';
                    break;

                case '.':
                    re[j++] = '\\';
                    re[j++] = '.';
                    break;

                case '[':
                    re[j++] = '[';
                    i++;
                    while (i < s.length() && s[i] != ']')
                        re[j++] = s[i++];
                    re[j++] = ']';
                    break;

                default:
                    re[j++] = s[i];
                    break;
            }
            i++;
            ASSERT(j < len);
        }
        if (shell) {
            re[j++] = '$';
        }
        re[j] = 0;
        str_  = re;
    }
}

void Regex::print(std::ostream& s) const {
    s << "/" << str_ << "/";
}

bool Regex::match(const std::string& s) const {
    return std::regex_match(s, std::regex(str_));
}


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics
