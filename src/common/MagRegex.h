/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Regex.h
// Baudouin Raoult - ECMWF Jan 98

#ifndef magics_Regex_h
#define magics_Regex_h


#include <string>


namespace magics {

//--------------------------------------------------------------------------------------------------

class Regex {
public:
    // -- Contructors

    Regex(const std::string& = ".*", bool shell = false);


    // -- Methods

    Regex& operator=(const Regex&);

    bool match(const std::string& s) const;

    operator const std::string&() const { return str_; }


protected:  // methods
    void print(std::ostream&) const;

private:  // members
    std::string str_;

private:  // methods
    void compile(const char*);

    friend std::ostream& operator<<(std::ostream& s, const Regex& p) {
        p.print(s);
        return s;
    }
};


//--------------------------------------------------------------------------------------------------


}  // namespace magics

#endif
