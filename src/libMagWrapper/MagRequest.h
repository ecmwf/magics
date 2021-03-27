/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagRequest.h
    \brief Definition of the Template class MagRequest.

    Magics Team - ECMWF 2010

    Started: Tue 24-Aug-2010

    Changes:

*/

#ifndef XMagRequest_H
#define XMagRequest_H

#include "magics.h"

namespace magics {

class MagParam {
public:
    MagParam() {}
    ~MagParam() {}

    virtual operator double() const { return dvalue_; }
    virtual operator string() const { return svalue_; }
    virtual operator int() const { return ivalue_; }
    virtual operator long int() const { return lvalue_; }

    virtual MagParam& operator=(int i) {
        ivalue_ = i;
        cout << "set int" << ivalue_ << endl;
        return *this;
    }
    virtual MagParam& operator=(string s) {
        svalue_ = s;
        cout << "set string" << svalue_ << endl;
        return *this;
    }
    virtual MagParam& operator=(double d) {
        dvalue_ = d;
        cout << "set double" << dvalue_ << endl;
        return *this;
    }
    virtual MagParam& operator=(long int i) {
        ivalue_ = i;
        cout << "set long int" << ivalue_ << endl;
        return *this;
    }

protected:
    string svalue_;
    int ivalue_;
    int lvalue_;
    double dvalue_;
};


class MagRequest {
public:
    MagRequest() {}
    virtual ~MagRequest() {}


    virtual string getVerb() const                                = 0;
    virtual MagParam& operator()(const string& name) const        = 0;
    virtual MagParam& operator()(const string& name, int i) const = 0;
    virtual int countValues(const string& name) const             = 0;

    virtual MagRequest& getSubRequest(const string&) = 0;
    virtual operator bool() const                    = 0;
    virtual void advance()                           = 0;
    virtual int countParameters()                    = 0;
    virtual int countValues(const string&)           = 0;
    virtual string getParameter(int)                 = 0;
    virtual void print()                             = 0;
    virtual void read(const string&)                 = 0;
    virtual MagRequest& justOneRequest()             = 0;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const {}


private:
    //! Copy constructor - No copy allowed
    // MagRequest(const MagRequest&);
    //! Overloaded << operator to copy - No copy allowed
    // MagRequest& operator=(const MagRequest&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MagRequest& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
