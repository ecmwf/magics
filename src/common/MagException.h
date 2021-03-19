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
    MagicsException(const string& why = "");

    void reason(const std::string& what) { what_ = what; }

    virtual const char* what() const throw() override { return what_.c_str(); }
    virtual ~MagicsException() throw() {}

    static std::string syserror();

private:
    virtual void print(ostream& out) const { out << what_; }
    string what_;
    // -- Friends
    friend ostream& operator<<(ostream& s, const MagicsException& p) {
        p.print(s);
        return s;
    }
};


class CannotOpenFile : public MagicsException {
public:
    CannotOpenFile(const string& file);
};


class NotSupported : public MagicsException {
public:
    NotSupported(const string& msg) : MagicsException(msg) {}
};


class NotImplemented : public MagicsException {
public:
    NotImplemented(const string& msg);
};

class NotYetImplemented : public MagicsException {
public:
    NotYetImplemented(const string& type, const string& method);
};

class MethodNotYetImplemented : public NotYetImplemented {
public:
    MethodNotYetImplemented(const string& method);
};

class ParameterNotYetImplemented : public NotYetImplemented {
public:
    ParameterNotYetImplemented(const string& param);
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
