/***************************** LICENSE START ***********************************

 Copyright 2012 ECMWF and INPE. This software is distributed under the terms
 of the Apache License version 2.0. In applying this license, ECMWF does not
 waive the privileges and immunities granted to it by virtue of its status as
 an Intergovernmental Organization or submit itself to any jurisdiction.

 ***************************** LICENSE END *************************************/

// inc_iostream.h                                 040727/vk, revised 110812/ir

// include and set up iostream facilities


#ifndef _INC_IOSTREAM_H_
#define _INC_IOSTREAM_H_

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <strstream>

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::ends;
using std::flush;
using std::ifstream;
using std::ios;
using std::istream;
using std::istringstream;
using std::istrstream;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::ostrstream;
using std::setfill;
using std::setw;
using std::streambuf;

// using std:: ;

typedef ios::openmode tOpenMode;


// we need to repeat this from mars.h because fstream
// includes cstdio which redefines these functions and kills the
// support for >2GB files (on some filesystems). We can detect this by
// checking whether fopen() has been undefined.

#if !defined(fopen)
#ifdef LARGE_FILES_SUPPORT
#ifdef linux
#define fopen(a, b) (FILE*)fopen64(a, b)
#else
#define fopen(a, b) fopen64(a, b)
#endif
#define fseek(a, b, c) fseeko64(a, b, c)
#define ftell(a) ftello64(a)
#define fsetpos(a, b) fsetpos64(a, b, c)
#define fgetpos(a, b) fgetpos64(a, b)
#define lseek(a, b, c) lseek64(a, b, c)
#define lstat(a, b) lstat64(a, b)
#define fstat(a, b) fstat64(a, b)
#define stat stat64
#endif
#endif

// end of repeat from mars.h


#endif
// _INC_IOSTREAM_H_
