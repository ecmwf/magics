/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdio.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>

#include <string.h>

#include <new>
#include <iostream>
#include <fstream>
#include <iomanip>


#include <sstream>
#include <functional>
#define HAS_STRINGSTREAM
/* #include <strstream.h> */

#ifndef _THREAD_SAFE
#define _THREAD_SAFE
#endif

// Stl

#include <set>
#include <map>
#include <list>
#include <vector>
#include <stack>
#include <algorithm>
#include <memory>
#include <string>
#include <numeric>
#include <queue>


using namespace std;

#define __FUNCTION__  __FILE__

#define STATIC_CAST(a,b)      static_cast<a>(b)
#define DYNAMIC_CAST(a,b)     dynamic_cast<a>(b)
#define REINTERPRET_CAST(a,b) reinterpret_cast<a>(b)

#define NPOS string::npos
#define THROW_NOTHING throw

#define HAS_BOOL


#ifndef linux
#define linux
#endif

//typedef int   fortint;
//typedef float fortfloat;

#ifndef __USE_LARGEFILE64
#define off64_t     off_t
#define stat64      stat
#define ftruncate64 ftruncate
#define flock64     flock
#define lseek64     lseek
#define open64      open
#define fopen64     fopen
#define fseeko64    fseek
#define ftello64    ftell
#define mmap64      mmap
#define fstat64     fstat
#endif

#include <stdint.h>

#define INCLUDE_TEMPLATES

#if defined(__GNUC__) && __GNUC__ < 3
/* #define OLD_STREAMBUF */
#else


#define HAS_STRINGSTREAM

struct output_iterator {
  typedef output_iterator_tag iterator_category;
  typedef void                value_type;
  typedef void                difference_type;
  typedef void                pointer;
  typedef void                reference;
};

// GCC 3.3 is confused about offsetof
//static double _offset;
#define member_offset(Z,z)  size_t( reinterpret_cast<char*>(&reinterpret_cast<Z*>(&_offset)->z) - reinterpret_cast<char*>(&_offset))
#define member_size(Z,z)    size_t( sizeof(reinterpret_cast<Z*>(&_offset)->z))


#endif

