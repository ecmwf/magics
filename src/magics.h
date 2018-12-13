/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file magics.h

  \brief This file contains all global definition for the MagPlus library.
  \author Meteorological Visualisation Section, ECMWF
  \license Apache License 2.0

  Started: January 2004

  Changes:

*/
#ifndef magicsplusplus_H
#define magicsplusplus_H

#include <magics_config.h>
#if defined(MAGICS_AIX_XLC)
 #include <unistd.h>           // for AIX
#endif
#include <climits>

// do the export restriction only if NOT for Metview
#ifndef HAVE_METVIEW

#ifdef WIN32
  #define MAGICS_NO_EXPORT
  #define MAGICS_IMPORT __declspec(dllimport)
  #define MAGICS_EXPORT __declspec(dllexport)
  #define MAGICS_DLLLOCAL
  #define MAGICS_DLLPUBLIC
#elif (__GNUC__ - 0 > 3) || (__GNUC__ - 0 == 3 && __GNUC_MINOR__ - 0 > 2)
  #define MAGICS_GCC 4
  #define MAGICS_NO_EXPORT __attribute__ ((visibility("hidden")))
  #define MAGICS_EXPORT    __attribute__ ((visibility("default")))
  #define MAGICS_IMPORT    __attribute__ ((visibility("default")))
  #define MAGICS_DLLLOCAL  __attribute__ ((visibility("hidden")))
  #define MAGICS_DLLPUBLIC __attribute__ ((visibility("default")))
#else
  #define MAGICS_GCC 3
  #define MAGICS_IMPORT
  #define MAGICS_EXPORT
  #define MAGICS_DLLLOCAL
  #define MAGICS_DLLPUBLIC
#endif

#else
  #define MAGICS_GCC 3
  #define MAGICS_NO_EXPORT
  #define MAGICS_IMPORT
  #define MAGICS_EXPORT
  #define MAGICS_DLLLOCAL
  #define MAGICS_DLLPUBLIC
#endif

#ifdef __GNUC__
#define MAGICS_DEPRECATED __attribute__((deprecated))
#else
#define MAGICS_DEPRECATED
#endif

#include <cstdlib>

#include <iostream>
#include <cstring>
#include <string>
#include <sstream>

using std::string;
using std::ostream;
using std::endl;
using std::ends;
using std::cout;
using std::cerr;
using std::ostringstream;
using std::ofstream;
using std::ifstream;

#include <vector>
#include <memory>
#include <map>
#include <set>
#include <numeric>

using std::allocator;
using std::vector;
using std::pair;
using std::map;
using std::set;
using std::unique_ptr;

#include <exception>

using std::exception;

#include <cassert>
#include <cmath>

#include "magics_windef.h"

#ifdef MAGICS_ON_WINDOWS
  #include <io.h>
  #define strcasecmp _stricmp
#endif

/*! \namespace magics

 The <I>magics</I> namespace encapsulates all elements of Magics++.
 The namespace prevents confusions with other libraries or 
 older versions of Magics.
 
 You can use this namespace by simple prefix all Magics elements
 by <I>magics::</I> or by typing <I>using namespace magics;</I>
 before any calls in your program.

*/
namespace magics {

template <class P>
class magvector : public std::vector<P>
{
public:
    magvector() {}
    magvector(const std::vector<P>& list) :  std::vector<P>(list) {}
    virtual ~magvector() {} 
    virtual MAGICS_NO_EXPORT void print(ostream& out) const
    {
        if ( this->size() < 10 ) {
            out << "Vector[";
            string sep = "";
            const unsigned int si = this->size();
            for (unsigned int i = 0; i < si; i++) {
                out << sep << (*this)[i];
                sep = ", ";
            }
            out << "]";
        }
        else {
            unsigned int nb = this->size();
            out << "Vector[" << (*this)[0] << ", " << (*this)[1] << ", " << (*this)[2];
            out << "...." << (*this)[nb-3] << ", " << (*this)[nb-2]  << ", " << (*this)[nb-1];
            out << "(" << nb << " elements)]";
        }
    }
// -- Friends
    friend MAGICS_NO_EXPORT ostream& operator<<(ostream& s,const magvector<P>& p)
        { p.print(s); return s; }
};


typedef magvector<string>   stringarray;
typedef magvector<int>      intarray;
typedef magvector<long int> longintarray;
typedef magvector<double>   doublearray;
typedef magvector<double>   floatarray;

enum LineStyle { M_SOLID , M_DASH , M_DOT , M_CHAIN_DASH , M_CHAIN_DOT };
enum Hemisphere { NORTH , SOUTH };
enum Justification { MLEFT, MCENTRE, MRIGHT };
enum Position { M_AUTOMATIC, M_TOP , M_BOTTOM , M_LEFT, M_RIGHT};
enum VerticalAlign { MNORMAL, MTOP, MCAP, MHALF, MBASE, MBOTTOM };
enum Shading { M_SH_NONE, M_SH_SOLID, M_SH_HATCH, M_SH_DOT };
enum ArrowPosition { M_TAIL, M_CENTRE, M_HEAD_ONLY};
enum DisplayType { ABSOLUTE, INLINE, BLOCK, NONE, HIDDEN };
enum ListPolicy { M_LASTONE, M_CYCLE };
enum GraphicsFormat {PS, EPS, PDF, SVG, KML, PNG, X, CPS, CSVG, GIF, AGIF, JPG, QT, GEOJSON};
enum AxisAutomaticSetting {m_off, m_both, m_min_only, m_max_only};

static  double EPSILON = 1.25e-10;

template<class T>
inline MAGICS_NO_EXPORT T abs(const T a) { return (a < 0) ? -a : a; }

#ifndef PI
const double PI = atan(1.)*4.;
#endif

inline MAGICS_NO_EXPORT double RAD(const double r) { return r*PI/180.;}
inline MAGICS_NO_EXPORT double DEG(const double d) { return d*180./PI;}
inline MAGICS_NO_EXPORT bool zero(const double v) { return abs(v) < EPSILON; }
inline MAGICS_NO_EXPORT bool same(const double a, const double b) { return zero(a-b); }
inline MAGICS_NO_EXPORT bool zero(const double v, double epsilon) { return abs(v) < epsilon; }
inline MAGICS_NO_EXPORT bool same(const double a, const double b, double epsilon) { return zero(a-b, epsilon); }

//! Global function to read env variables
inline MAGICS_NO_EXPORT string getEnvVariable(const string var)
{
    const char* va = var.c_str();
    const char* ww = getenv(va);
    if(ww) return string(ww);
    if(!strcmp(va,"MAGPLUS_HOME")) return string(MAGICS_INSTALL_PATH);
    return "";
}

//! Global function to return the Magics++ version for ID line
/*! comes from magics_config.h !!! */
inline string getMagicsVersionString()
{
    const string magics  = MAGICS_NAME;
    string version = MAGICS_VERSION_STR;
    if ( sizeof(long)==8)
         version += string(" (64 bit)");
    return magics + string(" ") + version;
}

// inline MAGICS_NO_EXPORT int upper_case(const int c) { return toupper(c);}
inline MAGICS_NO_EXPORT char lower_case(const char c) { return tolower(c);}

//! Make an lowercase copy of s:
inline MAGICS_NO_EXPORT string lowerCase(const string& s)
{
    std::string out;
    std::string::const_iterator se = s.end();
    for ( string::const_iterator l = s.begin(); l != se; ++l)
    {
        char ii = tolower(*l);
        out.push_back(ii);
    }
    return out;
}

/*!
  \brief compares two strings
*/
inline MAGICS_NO_EXPORT bool magCompare(const string &s1, const string &s2)
{
    if(s1.size() != s2.size()) return false;
    return !( strcasecmp(s1.c_str(),s2.c_str()) );
}

inline MAGICS_NO_EXPORT std::string replacePathWithHome(const string & path)
{
    const std::string home_path = getEnvVariable("HOME");
    std::string filename = path.substr(path.find_last_of("/\\"));
    return home_path + filename;
}

/*!
  \brief returns the biggest integer inside a double
*/
inline double maground(double x)
{
    return floor(x + 0.5);
}

inline double tonumber(const string& str) 
{
    double r;
    std::stringstream ss(str);
    ss >> r;
    return r;
}

template <class T>
inline string tostring(const T& in) 
{
    std::ostringstream out;
    out << in;
    return out.str();
}

#define MAGPLUS_PATH_TO_SHARE_ "/share/magics/"
#define MAGPLUS_LINK_  "http://software.ecmwf.int/magics"
#define MAGPLUS_PATH_TO_PS_FONTS_ POSTSCRIPT_FONT_PATH;
}
#endif
