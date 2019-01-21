/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagJSon.h
    \brief Definition of the Template class MagJSon.

    Magics Team - ECMWF 2007

    Started: Tue 3-Apr-2007

    Changes:

*/

#ifndef MagJSon_H
#define MagJSon_H


#include "WebFormat.h"
#include "XmlTree.h"
#include "json_spirit.h"

namespace magics {


class MagJSon : public WebFormat {
public:
    MagJSon();
    ~MagJSon() {}

    void execute(const string&, const map<string, string>&);


    void magics(const json_spirit::Value&);
    void build(XmlNode& parent, const string&, json_spirit::Object& object);
    typedef void (MagJSon::*Patch)(XmlNode&, const json_spirit::Value&);

    map<string, Patch> patchs_;
    XmlTree tree_;

    void drivers(XmlNode& parent, const json_spirit::Value&);
    void definitions(XmlNode& parent, const json_spirit::Value& value);
    void interpret(const string&);

protected:
    void print(ostream&) const {}
    void parse(const string&);
};

class ParamJSon : public map<string, string> {
public:
    ParamJSon(const string&);
    ~ParamJSon() {}
    string get(const string&, const string&);
    bool hasKey(const string&);

protected:
    void magics(const json_spirit::Value&);
};


}  // namespace magics
#endif
