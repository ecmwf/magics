/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#ifndef ProjP_H
#define ProjP_H


#include "magics.h"

typedef struct PJconsts PJ;


namespace magics {

class ProjP {
public:
    ProjP();
    ProjP(const string&, const string&);

    virtual ~ProjP();

    bool valid() const { return converter_; }

    static string error(int error);

    virtual int convert(double&, double&) const;
    virtual int revert(double&, double&) const;

protected:
    //! Method to print string about this class on to a stream of type ostream
    //! (virtual).
    virtual void print(ostream&) const;
    string from_;
    string to_;
    PJ* converter_;

private:
    //! Copy constructor - No copy allowed
    ProjP(const ProjP&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ProjP& p) {
        p.print(s);
        return s;
    }
};

class LatLonProjP : public ProjP {
public:
    LatLonProjP() {}
    ~LatLonProjP() override {}
    LatLonProjP(const string& to) : ProjP("EPSG:4326", to) {}
    int convert(double&, double&) const override;
    int revert(double&, double&) const override;
};

}  // namespace magics
#endif
