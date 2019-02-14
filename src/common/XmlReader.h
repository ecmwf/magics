/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file XmlReader.h
    \brief Definition of the Template class XmlReader.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jun-2005

*/

#ifndef XmlReader_H
#define XmlReader_H

#include <stack>

#include "XmlTree.h"
#include "magics.h"

namespace magics {

class XmlReader : private std::stack<XmlNode*> {
public:
    MAGICS_EXPORT XmlReader(bool tag = false);
    MAGICS_EXPORT virtual ~XmlReader();
    MAGICS_EXPORT void interpret(const string&, XmlTree*);
    int decode(const string&, XmlTree*);
    void newElement(const string&, const map<string, string>&);
    void endElement(const string&);
    void addData(const string&);

    bool dataAsTag() { return dataAsTag_; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    XmlTree* tree_;
    bool dataAsTag_;

private:
    //! Copy constructor - No copy allowed
    XmlReader(const XmlReader&);
    //! Overloaded << operator to copy - No copy allowed
    XmlReader& operator=(const XmlReader&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const XmlReader& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
