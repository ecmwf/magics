/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File Path
// Sylvie Lamy-Thepaut - ECMWF Mar 02

#ifndef Path_H
#define Path_H


#include "MagTranslator.h"
#include "magics.h"


namespace magics {


class XmlNode;

class Path {
public:
    Path() {}
    Path(const string& path) {
        // remove the "white" space at the end of the string"
        int index = path.find_last_not_of(" ");
        path_     = path.substr(0, index + 1);
    }
    void set(const map<string, string>&) {}
    void set(const XmlNode&) {}
    void toxml(ostream&, int) const {}
    Path* clone() const { return new Path(path_); }
    // -- Destructor
    virtual ~Path() {}

    // -- Convertors
    operator const string&() const { return path_; }
    operator const char*() const { return path_.c_str(); }


protected:
    virtual void print(ostream& out) const { out << "Path = " << path_; }

private:
    string path_;

    // -- Friends
    friend ostream& operator<<(ostream& s, const Path& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, Path> {
public:
    Path* operator()(string value) { return new Path(value); }

    Path* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics

#endif
