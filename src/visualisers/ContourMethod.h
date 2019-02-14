/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ContourMethod.h
    \brief Definition of the Template class ContourMethod.

    Magics Team - ECMWF 2004

    Started: Thu 11-Mar-2004

    Changes:

*/

#ifndef ContourMethod_H
#define ContourMethod_H

#include "MatrixHandler.h"
#include "PointsHandler.h"
#include "magics.h"

namespace magics {

class BasicGraphicsObjectContainer;
class XmlNode;


class ContourMethod {
public:
    ContourMethod() {}
    virtual ~ContourMethod() {}
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    void toxml(ostream&) const {}

    virtual ContourMethod* clone() const { return new ContourMethod(); }
    virtual MatrixHandler* handler(const AbstractMatrix& matrix, const BasicGraphicsObjectContainer&) {
        return new MatrixHandler(matrix);
    }
    virtual bool needPoints() { return false; }
    virtual MatrixHandler* handlePoints(const AbstractPoints&, const Layout&) {
        throw MethodNotYetImplemented("ContourMethod<P>::handler(const AbstractPoints<P>& matrix, const Layout&)");
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "ContourMethod[]"; }

private:
    //! Copy constructor - No copy allowed
    ContourMethod(const ContourMethod&);
    //! Overloaded << operator to copy - No copy allowed
    ContourMethod& operator=(const ContourMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ContourMethod& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, ContourMethod> {
public:
    ContourMethod* operator()(const string& val) { return SimpleObjectMaker<ContourMethod>::create(val); }

    ContourMethod* magics(const char* param) {
        ContourMethod* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
    ContourMethod* magics(const string& param) {
        ContourMethod* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};
}  // namespace magics

#endif
