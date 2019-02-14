/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagicsException_H
#define MagicsException_H

#include "magics.h"


namespace magics {

class MagicsException : public exception {
public:
    MagicsException(const string& why) : what_(why) {
        if (std::getenv("MAGICS_ABORT_EXCEPTION")) {
            std::abort();
        }
    }

    MagicsException() : what_("") {
        if (std::getenv("MAGICS_ABORT_EXCEPTION")) {
            std::abort();
        }
    }

    virtual const char* what() const throw() { return what_.c_str(); }
    virtual ~MagicsException() throw() {}

protected:
    virtual void print(ostream& out) const { out << what_; }
    string what_;
    // -- Friends
    friend ostream& operator<<(ostream& s, const MagicsException& p) {
        p.print(s);
        return s;
    }
};


class NoSuchFileException : public MagicsException {
public:
    NoSuchFileException(const string& file) : MagicsException("No Such File: " + file) {}
};

class NoWritePermissionException : public MagicsException {
public:
    NoWritePermissionException(const string& file) : MagicsException("No write permission to write file: " + file) {}
};

class NotYetImplemented : public MagicsException {
public:
    NotYetImplemented(const string& type, const string& method) :
        MagicsException(type + " " + method + " : not yet implemented... ") {}
};

class MethodNotYetImplemented : public NotYetImplemented {
public:
    MethodNotYetImplemented(const string& method) : NotYetImplemented("Method", method) {}
};

class ParameterNotYetImplemented : public NotYetImplemented {
public:
    ParameterNotYetImplemented(const string& param) : NotYetImplemented("Parameter", param) {}
};

class AssertionFailed : public MagicsException {
public:
    AssertionFailed(const string&);
    AssertionFailed(const char*, int, const char*, const char*);
};


inline void Assert(int code, const char* msg, int line, const char* file, const char* proc) {
    if (code != 0)
        throw AssertionFailed(msg, line, file, proc);
}


class IgnoreException : public MagicsException {
public:
    IgnoreException() : MagicsException("Just Ignore...") {}
};

}  // end namespace magics

#ifndef ASSERT
#define ASSERT(a) Assert(!(a), #a, __LINE__, __FILE__, __FUNCTION__)
#endif

#ifndef NOTIMP
#define NOTIMP throw MethodNotYetImplemented(__FUNCTION__)
#endif

#endif
// EXCEPTION_H_
