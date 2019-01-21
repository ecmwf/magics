/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef Tokenizer_H
#include "Tokenizer.h"
#endif

#include <algorithm>
#include <fstream>

Tokenizer::Tokenizer(const string& separators) {
    for (unsigned int i = 0; i < separators.length(); i++)
        separator_.insert(separators[i]);
}

Tokenizer::~Tokenizer() {}


void Tokenizer::operator()(const string& raw, vector<string>& v) {
    int index    = 0;
    int length   = raw.length();
    string token = "";

    while (index < length) {
        char c = raw[index];
        if (find(separator_.begin(), separator_.end(), c) != separator_.end()) {
            if (token.length() > 0)
                v.push_back(token);
            token = "";
        }
        else
            token += c;

        index++;
    }

    if (token.length() > 0)
        v.push_back(token);
}

void Tokenizer::operator()(std::istream& in, vector<string>& v) {
    string raw;
    char c;

    while (in.get(c) && c != EOF && c != '\n')
        raw += c;

    operator()(raw, v);
}
