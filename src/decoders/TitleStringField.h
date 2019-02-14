/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TitleStringField.h
    \brief Definition of the Template class TitleStringField.

    Magics Team - ECMWF 2004

    Started: Mon 21-Jun-2004

    Changes:

*/

#ifndef TitleStringField_H
#define TitleStringField_H

#include "magics.h"

#include "TitleField.h"

namespace magics {

class TitleStringField : public TitleField {
public:
    TitleStringField(const string& text);
    virtual ~TitleStringField();
    virtual string name() { return "text"; }

    virtual void operator()(vector<string>&) const;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string text_;

private:
    //! Copy constructor - No copy allowed
    TitleStringField(const TitleStringField&);
    //! Overloaded << operator to copy - No copy allowed
    TitleStringField& operator=(const TitleStringField&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const TitleStringField& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
