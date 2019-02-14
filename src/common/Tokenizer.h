/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Tokenizer.h
// Manuel Fuentes - ECMWF Jan 97

#ifndef Tokenizer_H
#define Tokenizer_H

// Headers

#include "magics.h"


//

class Tokenizer {
public:
    // -- Contructors

    Tokenizer(const string&);

    // -- Destructor

    ~Tokenizer();  // Change to virtual if base class

    // -- Methods

    void operator()(const string&, vector<string>&);
    void operator()(std::istream&, vector<string>&);

private:
    // No copy allowed

    Tokenizer(const Tokenizer&);
    Tokenizer& operator=(const Tokenizer&);

    // -- Members

    set<char, std::less<char> > separator_;  // To make searching faster

    // -- Methods

    void print(ostream&) const;

    friend ostream& operator<<(ostream& s, const Tokenizer& p) {
        p.print(s);
        return s;
    }
};


#endif
