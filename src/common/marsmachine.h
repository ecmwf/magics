/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef marsmachine_H
#define marsmachine_H

// All machine dependant stuff should go here

#ifdef __xlC__
#include "aix.h"
#endif

#ifdef __GNUG__
#include "linux.h"
#endif

#ifdef __sgi
#include "sgi.h"
#endif

#ifdef __hpux
#include "hpux.h"
#endif

#ifdef _WIN32
#include "magics_windows.h"
#endif

// Usefull macros

#ifndef NUMBER
#define NUMBER(x) (sizeof(x)/sizeof(x[0]))
#endif

template<class T>
inline void zero(T& p) { ::memset(&p,0,sizeof(T)); }

class Bless {
public:	

	bool          operator() (bool*  a)          { return *a; }
	int           operator() (int*   a)          { return *a; }
	short         operator() (short*   a)          { return *a; }
	char          operator() (char*  a)          { return *a; }
	long          operator() (long* a)           { return *a; }
	long long     operator() (long long* a)      { return *a; }

	unsigned long operator() (unsigned long* a) { return *a; }
	unsigned int  operator() (unsigned int* a)  { return *a; }
	unsigned char operator() (unsigned char* a) { return *a; }
	unsigned short operator() (unsigned short* a) { return *a; }
	unsigned long long operator() (unsigned long long* a)      { return *a; }

	double        operator() (double* a)        { return *a; }

	template<class T>
	Bless&        operator() (T*)               { return *this; }
};

class TypeInfo;
class Isa {
public:
	Isa* next_;
	TypeInfo* type_;
	Isa(TypeInfo* t,Isa* n) : next_(n), type_(t) {};
	static void add(TypeInfo* t,const string&);
	static Isa* get(const string&);  


};


#ifndef member_size
#define member_size(a,b)   size_t(sizeof(((a*)0)->b))
#endif

#ifndef member_offset
#define member_offset(a,b) size_t(&(((a*)0)->b))
#endif

class Schema {
public:
	virtual void start(const string&, size_t size) = 0;
	virtual void member(const string&, size_t size, size_t offset, const string& type) = 0;
	virtual void end(const string&) = 0;
};

template<class T>
void _describe(ostream& s,int depth,const T& what) 
{
	what.describe(s,depth);
}

void _describe(ostream& s,int depth,int what);
void _describe(ostream& s,int depth,unsigned int what);
void _describe(ostream& s,int depth,short what);
void _describe(ostream& s,int depth,bool what);
void _describe(ostream& s,int depth,unsigned short what);
void _describe(ostream& s,int depth,long what);
void _describe(ostream& s,int depth,long long what);
void _describe(ostream& s,int depth,unsigned long long what);
void _describe(ostream& s,int depth,unsigned long what);
void _describe(ostream& s,int depth,char what);
void _describe(ostream& s,int depth,unsigned char what);
void _describe(ostream& s,int depth,double what);

void _startClass(ostream& s,int depth,const string& name);
void _endClass(ostream& s,int depth,const string& name);
void _startMember(ostream& s,int depth,const string& name);
void _endMember(ostream& s,int depth,const string& name);

template<class T>
void _describe(ostream& s,int depth,const string& name,const T& what) 
{
	_startMember(s,depth,name);
	_describe(s,depth,what);
	_endMember(s,depth,name);
}


#endif

