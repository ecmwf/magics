/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   Translator.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   Jun 1996

#ifndef magics_Translator_h
#define magics_Translator_h

#include <set>
#include <string>
#include <vector>

namespace magics {

//----------------------------------------------------------------------------------------------------------------------


// Translate a type from From to To

template <class From, class To>
struct Translator {
    // Default template calls cast
    To operator()(const From& from) { return To(from); }
};

// Those are predefined

template <>
struct Translator<bool, std::string> {
    std::string operator()(bool);
};


template <>
struct Translator<std::string, bool> {
    bool operator()(const std::string&);
};


template <>
struct Translator<unsigned char, std::string> {
    std::string operator()(unsigned char);
};

template <>
struct Translator<float, std::string> {
    std::string operator()(float);
};

template <>
struct Translator<short, std::string> {
    std::string operator()(short);
};

template <>
struct Translator<int, std::string> {
    std::string operator()(int);
};

template <>
struct Translator<unsigned int, std::string> {
    std::string operator()(unsigned int);
};

template <>
struct Translator<std::string, int> {
    int operator()(const std::string&);
};

template <>
struct Translator<std::string, unsigned int> {
    unsigned int operator()(const std::string&);
};

template <>
struct Translator<double, std::string> {
    std::string operator()(double);
};

template <>
struct Translator<std::string, double> {
    double operator()(const std::string&);
};

template <>
struct Translator<std::string, float> {
    float operator()(const std::string&);
};

template <>
struct Translator<long, std::string> {
    std::string operator()(long);
};

template <>
struct Translator<std::string, long> {
    long operator()(const std::string&);
};

template <>
struct Translator<std::string, short> {
    short operator()(const std::string&);
};

template <>
struct Translator<unsigned long, std::string> {
    std::string operator()(unsigned long);
};

template <>
struct Translator<std::string, unsigned long> {
    unsigned long operator()(const std::string&);
};

template <>
struct Translator<std::string, unsigned char> {
    unsigned char operator()(const std::string&);
};

template <>
struct Translator<std::string, unsigned long long> {
    unsigned long long operator()(const std::string&);
};

template <>
struct Translator<std::string, long long> {
    long long operator()(const std::string&);
};

template <>
struct Translator<unsigned long long, std::string> {
    std::string operator()(unsigned long long);
};

template <>
struct Translator<long long, std::string> {
    std::string operator()(long long);
};

template <>
struct Translator<std::string, char> {
    char operator()(const std::string&);
};

template <>
struct Translator<char, std::string> {
    std::string operator()(char);
};

template <>
struct Translator<std::string, std::vector<std::string> > {
    std::vector<std::string> operator()(const std::string&);
};

template <>
struct Translator<std::string, std::vector<long> > {
    std::vector<long> operator()(const std::string&);
};

template <>
struct Translator<std::vector<long>, std::string> {
    std::string operator()(const std::vector<long>&);
};

template <>
struct Translator<std::vector<std::string>, std::string> {
    std::string operator()(const std::vector<std::string>&);
};

template <>
struct Translator<std::string, std::set<std::string> > {
    std::set<std::string> operator()(const std::string&);
};

template <>
struct Translator<std::set<std::string>, std::string> {
    std::string operator()(const std::set<std::string>&);
};


//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics

#endif
