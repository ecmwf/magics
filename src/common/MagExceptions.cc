/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <signal.h>

#ifndef MagExceptions_H
#include "MagExceptions.h"
#endif

#ifndef ThreadSingleton_H
#include "ThreadSingleton.h"
#endif

static MagException*& first()
{
	static ThreadSingleton<MagException*> p;
	return p.instance();
}

void xldb_throw(const char *c) // To set a break point in xldb
{
	if(getenv("PAUSE_MagExceptionS"))
	{
		std::cout << "debug me " << getpid() << endl;
		pause();
	}

	if(getenv("ABORT_MagExceptionS"))
		Panic(c);
}

MagException::MagException():
	next_(first())
{
	first() = this;
	xldb_throw("?");
}

MagException::~MagException() THROW_NOTHING()
{
	first() = next_;
}

void MagException::MagExceptionStack(ostream& out)
{
	out << "MagException stack: " << endl;
	MagException* e =  first();
	while(e)
	{
		out << e->what() << endl;
		e = e->next_;
	}
	out << "End stack" << endl;
}

MagException::MagException(const string& w):
	what_(w),
	next_(first())
{
	first() = this;
	xldb_throw(w.c_str());
}

void MagException::reason(const string& w)
{
	what_ = w;
}

bool MagException::throwing()
{
	return first() != 0;
}

TooManyRetries::TooManyRetries(const int retries) 
{
 cout << "Too many retries: " << retries << endl;
}

TimeOut::TimeOut(const unsigned long timeout)
{   
 cout << "Timeout expired: " << timeout << endl;
}


FailedSystemCall::FailedSystemCall(const string& w)
{   
 cout << "Failed system call: " << w << " " << endl;
}

FailedSystemCall::FailedSystemCall(const char* msg,int line, const char* file, const char* proc,int err)
{
 cout << "Failed system call: " << msg << " in " <<proc<< ", line " << line << " of " << file << endl;
}

AssertionFailed::AssertionFailed(const string& w): 
	MagException(string("Assertion failed: ") + w)
{   
} 

AssertionFailed::AssertionFailed(const char* msg,int line,
	const char* file, const char* proc)
{
 cout << "Assertion failed: " << msg << " in " <<proc<< ", line " << line << " of " << file << endl;
}

BadParameter::BadParameter(const string& w):
	MagException(string("Bad parameter: ") + w)
{   
}

NotImplemented::NotImplemented(int line,const char* file,const char* proc)
{
 cout << "Not implemented: " <<proc<< ", line " << line << " of " << file << endl;
}

UserError::UserError(const string& r):
	MagException(string("UserError: ") + r)
{   
}

UserError::UserError(const string& r,const string& x):
	MagException(string("UserError: ") + r + " : " + x)
{   
}

Stop::Stop(const string& r):
	MagException(string("Stop: ") + r)
{   
}

Abort::Abort(const string& r):
	MagException(string("Abort: ") + r)
{   
}

OutOfRange::OutOfRange(unsigned long long index, unsigned long long max)
{   
}

FileError::FileError(const string& msg)
{   
}

CantOpenFile::CantOpenFile(const string& file, bool retry):
	retry_(retry)
{   
}

WriteError::WriteError(const string& file): 
	FileError(string("Write error on ") + file)
{   
}

ReadError::ReadError(const string& file): 
	FileError(string("Read error on ") + file)
{   
}

ShortFile::ShortFile(const string& file): 
	ReadError(string("Short file while reading ") + file)
{   
}

Ostore::Ostore(const string& msg):
	MagException(string("ObjectStore: ") + msg)
{
}

void Panic(const char *msg)
{
	msg = msg ? msg : "(null message)";

	if(getenv("SLEEP_ON_PANIC"))
	{
		::kill(::getpid(),SIGSTOP);
	}
	else ::kill(::getpid(),SIGABRT);
	::pause();
}

void Panic(const char* msg,int line,const char* file, const char* proc)
{
}

OutOfMemory::OutOfMemory():
	MagException("out of memory")
{
}
