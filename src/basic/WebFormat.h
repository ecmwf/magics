/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagML.h
    \brief Definition of the Template class MagML.

    Magics Team - ECMWF 2007

    Started: Tue 3-Apr-2007

    Changes:

*/

#ifndef MagML_H
#define MagML_H


#include "XmlMagics.h"

namespace magics {

class TempFile {
public:
    TempFile() : filename(tmpnam(0)), ofs(filename) {
        if (!ofs)
            return;
    }

    ~TempFile() {
        ofs.close();
        remove(filename);
    }

    ofstream& operator()() { return ofs; }
    string name() { return filename; }

private:
    const char* filename;
    ofstream ofs;
};


class WebFormat {
public:
    WebFormat() {}
    ~WebFormat() {}


    void prepare(const string&, const map<string, string>&, TempFile& file);

    virtual void execute(const string&, const map<string, string>&) {}


protected:
    void print(ostream&) const {}

private:
    //! Copy constructor - No copy allowed
    WebFormat(const WebFormat&);
    //! Overloaded << operator to copy - No copy allowed
    WebFormat& operator=(const WebFormat&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const WebFormat& p) {
        p.print(s);
        return s;
    }
};

class MagML : public WebFormat {
public:
    MagML() {}
    ~MagML() {}

    void execute(const string&, const map<string, string>&);

protected:
    void print(ostream&) const {}
};


class WebInterpretor : public map<string, string> {
public:
    WebInterpretor();
    ~WebInterpretor();

    MAGICS_EXPORT static void magml(const string&);
    MAGICS_EXPORT static void json(const string&);
    MAGICS_EXPORT static void set(const string& param, const string& value) { web_.insert(make_pair(param, value)); }
    MAGICS_EXPORT static map<string, string>& parameters() { return web_; }


protected:
    MAGICS_EXPORT static WebInterpretor web_;
};

}  // namespace magics
#endif
