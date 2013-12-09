/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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

