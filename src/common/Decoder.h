/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Decoder.h
    \brief Definition of the Abstract template class Decoder.

    Magics Team - ECMWF 2004

    Started: Fri 16-Jan-2004

    Changes:

*/

#ifndef Decoder_H
#define Decoder_H

#include "magics.h"


namespace magics {

class Decoder {
public:
    Decoder(){};
    virtual ~Decoder(){};
    //! Method to decode : abstract
    virtual void decode() = 0;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "Base class Decoder"; }

private:
    //! Copy constructor - No copy allowed
    Decoder(const Decoder&);
    //! Overloaded << operator to copy - No copy allowed
    Decoder& operator=(const Decoder&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const Decoder& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
