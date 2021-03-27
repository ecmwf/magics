/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagMagExceptions_H
#define MagMagExceptions_H

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

#ifndef MagLog_H
#include "MagLog.h"
#endif

void Panic(const char*);
void Panic(const char* msg, int line, const char* file, const char* proc);

// General purpose MagException
// Other MagExceptions must be defined in the class that throw them

// Misc. errors

class MagException : public exception {
public:
    virtual const char* what() const THROW_NOTHING() { return what_.c_str(); }
    MagException(const string&);
    ~MagException() THROW_NOTHING();
    virtual bool retryOnServer() const { return false; }
    virtual bool retryOnClient() const { return false; }
    virtual bool terminateApplication() const { return false; }

    static bool throwing();
    static void MagExceptionStack(ostream&);

protected:
    void reason(const string&);
    MagException();

private:
    string what_;
    // SaveStatus save_;
    MagException* next_;
};

class SeriousBug : public MagException {
public:
    SeriousBug(const string& w) : MagException(string("Serious Bug:") + w) {}
    SeriousBug(const string&, const string&);
    SeriousBug(const string&, int);
};

class TooManyRetries : public MagException {
public:
    TooManyRetries(const int);
};

class TimeOut : public MagException {
public:
    TimeOut(const unsigned long);
};

class FailedSystemCall : public MagException {
public:
    FailedSystemCall(const string&);
    FailedSystemCall(const char*, int, const char*, const char*, int);
};

class AssertionFailed : public MagException {
public:
    AssertionFailed(const string&);
    AssertionFailed(const char*, int, const char*, const char*);
};

class BadParameter : public MagException {
public:
    BadParameter(const string& s);
};

class NotImplemented : public MagException {
public:
    NotImplemented(int, const char*, const char*);
};

class Stop : public MagException {
public:
    Stop(const string&);
};

class Abort : public MagException {
public:
    Abort(const string&);
};

class Cancel : public MagException {
public:
    Cancel(const string&);
};

class UserError : public MagException {
public:
    UserError(const string&);
    UserError(const string&, const string&);
    UserError(const string&, int);
};

class OutOfRange : public MagException {
public:
    OutOfRange(unsigned long long, unsigned long long);
};

// File errors

class FileError : public MagException {
protected:
    FileError(const string&);
    FileError() {}
};

class CantOpenFile : public FileError {
    bool retry_;
    virtual bool retryOnServer() const { return retry_; }

public:
    CantOpenFile(const string&, bool retry = false);
};

class WriteError : public FileError {
public:
    WriteError(const string&);
};

class ReadError : public FileError {
public:
    ReadError(const string&);
};

class ShortFile : public ReadError {
public:
    ShortFile(const string&);
};

// ObjectStore

class Ostore : public MagException {
public:
    Ostore(const string&);
};

// =======================================

inline void SysCall(long long code, const char* msg, int line, const char* file, const char* proc) {
    if (code < 0)
        throw FailedSystemCall(msg, line, file, proc, errno);
}

inline void ThrCall(int code, const char* msg, int line, const char* file, const char* proc) {
    if (code != 0)  // Threads return errno in return code
        throw FailedSystemCall(msg, line, file, proc, code);
}

inline void Assert(int code, const char* msg, int line, const char* file, const char* proc) {
    /*if(code != 0)
        throw AssertionFailed(msg,line,file,proc);
    */
}

inline void Panic(int code, const char* msg, int line, const char* file, const char* proc) {
    if (code != 0)
        Panic(msg, line, file, proc);
}

//--------------------------------------------------------------
// For compatibility
//--------------------------------------------------------------
class OutOfMemory : public MagException {
    virtual bool terminateApplication() const { return true; }
    virtual const char* what() const THROW_NOTHING() { return "OutOfMemory"; }

public:
    OutOfMemory();
};

#define THRCALL(a) ThrCall(a, #a, __LINE__, __FILE__, __FUNCTION__)
#define SYSCALL(a) SysCall(a, #a, __LINE__, __FILE__, __FUNCTION__)
#define ASSERT(a) Assert(!(a), #a, __LINE__, __FILE__, __FUNCTION__)
#define PANIC(a) Panic((a), #a, __LINE__, __FILE__, __FUNCTION__)

#endif
