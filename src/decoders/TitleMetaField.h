/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TitleMetaField.h
    \brief Definition of the Template class TitleMetaField.

    Magics Team - ECMWF 2004

    Started: Mon 21-Jun-2004

    Changes:

*/

#ifndef TitleMetaField_H
#define TitleMetaField_H

#include "magics.h"

#include "TitleField.h"

namespace magics {

class TitleMetaField : public TitleField {
public:
    TitleMetaField(const string&);
    virtual ~TitleMetaField();

    virtual string name() { return token_; }


    virtual void operator()(vector<string>&) const;


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string token_;


private:
    //! Copy constructor - No copy allowed
    TitleMetaField(const TitleMetaField&);
    //! Overloaded << operator to copy - No copy allowed
    TitleMetaField& operator=(const TitleMetaField&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TitleMetaField& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
