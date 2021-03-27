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
/// @author Tiago Quintino
/// @date Sep 2012

#ifndef magics_StreamParser_h
#define magics_StreamParser_h

#include <set>
#include <MagException.h>


namespace magics {

//----------------------------------------------------------------------------------------------------------------------

class StreamParser {

public: // types

    class Error : public MagicsException {
    public:
        Error(const std::string& what, size_t line = 0);
    };

public: // methods

    StreamParser(std::istream& in, bool comments = false, const char* comment = "#");

    char peek(bool spaces = false);
    char next(bool spaces = false);

    void consume(char);
    void consume(const char*);

    void expect(const char*);
    void putback(char);

protected: // members

    size_t line_;
    size_t pos_;

private: // members

    StreamParser(const StreamParser&);
    StreamParser& operator=(const StreamParser&);

    std::istream& in_;

    bool comments_;
    std::set<char> comment_;

    char _get();
    char _peek();
    bool _eof();

};

//----------------------------------------------------------------------------------------------------------------------

} // namespace magics

#endif
