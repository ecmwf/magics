/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file InputMatrixInterpretor.h
    \brief Definition of the Template class InputMatrixInterpretor.

    Magics Team - ECMWF 2005

    Started: Fri 16-Sep-2005

    Changes:

*/

#ifndef InputMatrixInterpretor_H
#define InputMatrixInterpretor_H

#include "Factory.h"
#include "InputMatrixIrregularInterpretorAttributes.h"
#include "InputMatrixRegularInterpretorAttributes.h"
#include "MagTranslator.h"
#include "magics.h"


namespace magics {

class XmlNode;
class UserPoint;
class UserPoint;
class Transformation;


class InputMatrix;

class InputMatrixInterpretor {
public:
    InputMatrixInterpretor();
    virtual ~InputMatrixInterpretor();

    virtual InputMatrixInterpretor* clone() { return new InputMatrixInterpretor(); }
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual void toxml(ostream&) const {}
    virtual bool accept(const string&) { return false; }
    virtual Matrix* xyInterpret(Matrix*, const InputMatrix&) { return 0; }
    virtual Matrix* geoInterpret(Matrix*, const InputMatrix&) { return 0; }
    virtual void getReady(const Transformation&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    typedef void (InputMatrixInterpretor::*Mapper)();
    std::map<string, Mapper> mappers_;
    void upperLeft();
    void lowerLeft();
    void upperRight();
    void lowerRight();
    void upperLeftTransposed();
    void lowerLeftTransposed();
    void upperRightTransposed();
    void lowerRightTransposed();

private:
    //! Copy constructor - No copy allowed
    InputMatrixInterpretor(const InputMatrixInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    InputMatrixInterpretor& operator=(const InputMatrixInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const InputMatrixInterpretor& p) {
        p.print(s);
        return s;
    }
};


class InputMatrixRegularInterpretor : public InputMatrixRegularInterpretorAttributes, public InputMatrixInterpretor {
public:
    InputMatrixRegularInterpretor();
    virtual ~InputMatrixRegularInterpretor();

    virtual void set(const map<string, string>& map) { InputMatrixRegularInterpretorAttributes::set(map); }
    virtual void set(const XmlNode& node) { InputMatrixRegularInterpretorAttributes::set(node); }

    virtual Matrix* xyInterpret(Matrix*, const InputMatrix&);
    virtual Matrix* geoInterpret(Matrix*, const InputMatrix&);
    virtual void getReady(const Transformation&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;
    string dateX_;  // Used to adjust the Matrix according to the projection
    string dateY_;  // Used to adjust the Matrix according to the projection

private:
    //! Copy constructor - No copy allowed
    InputMatrixRegularInterpretor(const InputMatrixRegularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    InputMatrixRegularInterpretor& operator=(const InputMatrixRegularInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const InputMatrixRegularInterpretor& p) {
        p.print(s);
        return s;
    }
};

class InputMatrixGaussianInterpretor : public InputMatrixRegularInterpretor {
public:
    InputMatrixGaussianInterpretor() {}
    virtual ~InputMatrixGaussianInterpretor() {}
};

class InputMatrixIrregularInterpretor : public InputMatrixIrregularInterpretorAttributes,
                                        public InputMatrixInterpretor {
public:
    InputMatrixIrregularInterpretor();
    virtual ~InputMatrixIrregularInterpretor();

    virtual void set(const map<string, string>& map) { InputMatrixIrregularInterpretorAttributes::set(map); }
    virtual void set(const XmlNode& node) { InputMatrixIrregularInterpretorAttributes::set(node); }

    virtual Matrix* geoInterpret(Matrix*, const InputMatrix&);
    virtual Matrix* xyInterpret(Matrix*, const InputMatrix&);

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    InputMatrixIrregularInterpretor(const InputMatrixIrregularInterpretor&);
    //! Overloaded << operator to copy - No copy allowed
    InputMatrixIrregularInterpretor& operator=(const InputMatrixIrregularInterpretor&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const InputMatrixIrregularInterpretor& p) {
        p.print(s);
        return s;
    }
};


template <>
class MagTranslator<string, InputMatrixInterpretor> {
public:
    InputMatrixInterpretor* operator()(const string& val) {
        return SimpleObjectMaker<InputMatrixInterpretor>::create(val);
    }

    InputMatrixInterpretor* magics(const string& param) {
        InputMatrixInterpretor* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
