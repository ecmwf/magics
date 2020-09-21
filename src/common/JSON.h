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
/// @date   Aug 2011

#ifndef magics_JSON_h
#define magics_JSON_h

#include <ctime>
#include <iosfwd>
#include <map>
#include <set>
#include <string>
#include <vector>



namespace magics {

class Time;
class Date;
class DateTime;

//----------------------------------------------------------------------------------------------------------------------

class JSON {

public:  // methods

    JSON(std::ostream&, bool null = true);

    ~JSON();

    JSON& operator<<(bool);
    JSON& operator<<(char);
    JSON& operator<<(unsigned char);
    JSON& operator<<(int);
    JSON& operator<<(unsigned int);
    JSON& operator<<(long);
    JSON& operator<<(unsigned long);
    JSON& operator<<(long long);
    JSON& operator<<(unsigned long long);
    JSON& operator<<(float);
    JSON& operator<<(double);

    JSON& operator<<(const Date&);
    JSON& operator<<(const Time&);
    JSON& operator<<(const DateTime&);
    JSON& operator<<(const ::timeval&);

    JSON& operator<<(const std::string&);
    JSON& operator<<(const char*);

    template <typename T>
    JSON& operator<<(const std::vector<T>&);

    template <typename T>
    JSON& operator<<(const std::set<T>&);

    template <typename T>
    JSON& operator<<(const std::map<std::string, T>&);

    JSON& null();

    JSON& startObject();
    JSON& endObject();

    JSON& startList();
    JSON& endList();

    /// Sets the precision for float and double (works like std::setprecision)
    JSON& precision(int);

    /// write raw characters into the json stream
    /// @warning use with care as this may create invalid json
    void raw(const char*, long);

private:  // members
    std::ostream& out_;
    std::vector<std::string> sep_;
    std::vector<bool> state_;
    bool null_;

private:  // methods
    void sep();
    bool inlist() { return !state_.back(); }
    bool indict() { return state_.back(); }
};

//----------------------------------------------------------------------------------------------------------------------

template <typename T>
JSON& JSON::operator<<(const std::vector<T>& v) {
    startList();
    for (size_t i = 0; i < v.size(); ++i) {
        *this << v[i];
    }
    endList();
    return *this;
}

template <typename T>
JSON& JSON::operator<<(const std::set<T>& s) {
    startList();
    for (typename std::set<T>::const_iterator i = s.begin(); i != s.end(); ++i) {
        *this << *i;
    }
    endList();
    return *this;
}

template <typename T>
JSON& JSON::operator<<(const std::map<std::string, T>& m) {
    startObject();

    for (typename std::map<std::string, T>::const_iterator it = m.begin(); it != m.end(); ++it)
        *this << (*it).first << (*it).second;

    endObject();
    return *this;
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace magics

#endif
