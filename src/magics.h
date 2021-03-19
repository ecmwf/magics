/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/*! \file magics.h

  \brief This file contains all global definition for the MagPlus library.
  \author Meteorological Visualisation Section, ECMWF
  \license Apache License 2.0

  Started: January 2004

  Changes:

*/

/*! \mainpage

 \section intro What is Magics?

 Magics is the second generation of a meteorological graphics
 library developed at the <a href="http://www.ecmwf.int"><i>European
 Centre for Medium Range Forecasts (ECMWF)</i></a>. This library is
 developed in C++ and offers output in various formats such as PostScript,
 PDF, PNG, SVG and Qt (for <a href="https://confluence.ecmwf.int/metview">Metview</a>).

 \section install How-to install

 Before installation you have to compile Magics. To do so, simply
 unpack the tarball in an appropiate directory and run <i>cmake ..</i>
 followed by <i>make</i>.

 To install type <i>make install</i>. Depending on the
 choosen installation directory you need root permission.

 \section interfaces Magics interfaces

 Magics offers different interfaces to access its graphics
 functionality: C++ (for higher-level applicatiosn such as Metview), C, Fortran 77
 Python and MagJson. The Fortran interface is intended to be backwards compatible with older
 versions (mainly the the 6.x series) of Magics.

 \section modules More information

 - \ref hilo "Computations of High &amp; Lows"

 - \ref projections "Geographical projections"

 - \ref drivers "Output drivers"

 - \ref coastlines "Coastlines"

 - \ref colours "Colours"

 - \ref obs "Plotting of observations"

 \section links Links

 - <a href="http://software.ecmwf.int/magics">Magics++ homepage</a>

 \section copyright License

 (C) Copyright 1996- ECMWF

 This software is licensed under the terms of the Apache Licence Version 2.0
 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 In applying this licence, ECMWF does not waive the privileges and immunities
 granted to it by virtue of its status as an intergovernmental organisation nor
 does it submit to any jurisdiction.
*/

#ifndef magicsplusplus_H
#define magicsplusplus_H

#include <magics_config.h>

#include <climits>

#include "magics_export.h"

#ifdef __GNUC__
#define MAGICS_DEPRECATED __attribute__((deprecated))
#else
#define MAGICS_DEPRECATED
#endif

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>

using std::cerr;
using std::cout;
using std::endl;
using std::ends;
using std::ifstream;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::string;

#include <map>
#include <memory>
#include <numeric>
#include <set>
#include <vector>

using std::allocator;
using std::map;
using std::pair;
using std::set;
using std::unique_ptr;
using std::vector;

#include <exception>

using std::exception;

// #include <cassert>
#include <cmath>

// #include "magics_windef.h"
#if defined(_WIN32) && defined(_MSC_VER)
#define MAGICS_ON_WINDOWS
#include <io.h>
inline int strcasecmp(const char* a, const char* b) {
    return _stricmp(a, b);
}
#endif

/*! \namespace magics

 The <I>magics</I> namespace encapsulates all elements of Magics.
 The namespace prevents confusions with other libraries or
 older versions of Magics.

 You can use this namespace by simple prefix all Magics elements
 by <I>magics::</I> or by typing <I>using namespace magics;</I>
 before any calls in your program.

*/
namespace magics {

template <class P>
class magvector : public std::vector<P> {
public:
    magvector() {}
    magvector(const std::vector<P>& list) : std::vector<P>(list) {}
    virtual ~magvector() {}
    virtual MAGICS_NO_EXPORT void print(ostream& out) const {
        if (this->size() < 10) {
            out << "Vector[";
            string sep            = "";
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
            out << "...." << (*this)[nb - 3] << ", " << (*this)[nb - 2] << ", " << (*this)[nb - 1];
            out << "(" << nb << " elements)]";
        }
    }
    // -- Friends
    friend MAGICS_NO_EXPORT ostream& operator<<(ostream& s, const magvector<P>& p) {
        p.print(s);
        return s;
    }
};

#define MAGPLUS_PATH_TO_SHARE_ "/share/magics/"
#define MAGPLUS_LINK_ "http://software.ecmwf.int/magics"
#define MAGPLUS_PATH_TO_PS_FONTS_ POSTSCRIPT_FONT_PATH;

typedef magvector<string> stringarray;
typedef magvector<int> intarray;
typedef magvector<long int> longintarray;
typedef magvector<double> doublearray;
typedef magvector<double> floatarray;

enum class LineStyle
{
    SOLID,
    DASH,
    DOT,
    CHAIN_DASH,
    CHAIN_DOT
};

std::ostream& operator<<(ostream& s, LineStyle);

enum class Hemisphere
{
    NORTH,
    SOUTH
};

std::ostream& operator<<(ostream& s, Hemisphere);

enum class Justification
{
    LEFT,
    CENTRE,
    RIGHT
};

std::ostream& operator<<(ostream& s, Justification);

enum class Position
{
    AUTOMATIC,
    TOP,
    BOTTOM,
    LEFT,
    RIGHT
};

std::ostream& operator<<(ostream& s, Position);

enum class VerticalAlign
{
    NORMAL,
    TOP,
    CAP,
    HALF,
    BASE,
    BOTTOM
};

std::ostream& operator<<(ostream& s, VerticalAlign);


enum class Shading
{
    NONE,
    SOLID,
    HATCH,
    DOT
};

std::ostream& operator<<(ostream& s, Shading);

enum class ArrowPosition
{
    TAIL,
    CENTRE,
    HEAD_ONLY
};

std::ostream& operator<<(ostream& s, ArrowPosition);

enum class DisplayType
{
    ABSOLUTE,
    INLINE,
    BLOCK,
    NONE,
    HIDDEN
};

std::ostream& operator<<(ostream& s, DisplayType);

enum class ListPolicy
{
    LASTONE,
    CYCLE
};

std::ostream& operator<<(ostream& s, ListPolicy);

enum class GraphicsFormat
{
    PS,
    EPS,
    PDF,
    SVG,
    KML,
    PNG,
    X,
    CPS,
    CSVG,
    GIF,
    AGIF,
    JPG,
    QT,
    GEOJSON
};

std::ostream& operator<<(ostream& s, GraphicsFormat);

enum class AxisAutomaticSetting
{
    OFF,
    BOTH,
    MIN_ONLY,
    MAX_ONLY
};

std::ostream& operator<<(ostream& s, AxisAutomaticSetting);

static /*const*/ double EPSILON = 1.25e-10;

template <class T>
inline MAGICS_NO_EXPORT T abs(const T a) {
    return (a < 0) ? -a : a;
}

#ifndef PI
const double PI = atan(1.) * 4.;
#endif

inline MAGICS_NO_EXPORT double RAD(const double r) {
    return r * PI / 180.;
}
inline MAGICS_NO_EXPORT double DEG(const double d) {
    return d * 180. / PI;
}
inline MAGICS_NO_EXPORT bool zero(const double v) {
    return abs(v) < EPSILON;
}
inline MAGICS_NO_EXPORT bool same(const double a, const double b) {
    return zero(a - b);
}
inline MAGICS_NO_EXPORT bool zero(const double v, double epsilon) {
    return abs(v) < epsilon;
}
inline MAGICS_NO_EXPORT bool same(const double a, const double b, double epsilon) {
    return zero(a - b, epsilon);
}

//! Global function to read env variables
inline MAGICS_NO_EXPORT string getEnvVariable(const string var) {
    const char* va = var.c_str();
    const char* ww = getenv(va);
    if (ww)
        return string(ww);
    if (!strcmp(va, "MAGPLUS_HOME"))
        return string(MAGICS_INSTALL_PATH);
    return "";
}

//! Global function to return the Magics version for ID line
/*! comes from magics_config.h !!! */
inline string getMagicsVersionString() {
    const string magics = MAGICS_NAME;
    string version      = MAGICS_VERSION_STR;
    return magics + string(" ") + version;
}

// inline MAGICS_NO_EXPORT int upper_case(const int c) { return toupper(c);}
inline MAGICS_NO_EXPORT char lower_case(const char c) {
    return tolower(c);
}

//! Make an lowercase copy of s:
inline MAGICS_NO_EXPORT string lowerCase(const string& s) {
    std::string out;
    std::string::const_iterator se = s.end();
    for (string::const_iterator l = s.begin(); l != se; ++l) {
        char ii = tolower(*l);
        out.push_back(ii);
    }
    return out;
}

/*!
  \brief compares two strings
*/
inline MAGICS_NO_EXPORT bool magCompare(const string& s1, const string& s2) {
    if (s1.size() != s2.size())
        return false;
    return !(strcasecmp(s1.c_str(), s2.c_str()));
}

inline MAGICS_NO_EXPORT std::string replacePathWithHome(const string& path) {
    const std::string home_path = getEnvVariable("HOME");
    std::string filename        = path.substr(path.find_last_of("/\\"));
    return home_path + filename;
}

inline MAGICS_NO_EXPORT string buildConfigPath(const string& config, const string& aux = "") {
    ostringstream out;
    out << getEnvVariable("MAGPLUS_HOME") << "/share/magics/" << config;
    if (aux.size())
        out << "/" << aux;
    return out.str();
}

/*!
  \brief returns the biggest integer inside a double
*/
inline double maground(double x) {
    return floor(x + 0.5);
}

inline double tonumber(const string& str) {
    double r;
    std::stringstream ss(str);
    ss >> r;
    return r;
}

template <class T>
inline string tostring(const T& in) {
    std::ostringstream out;
    out << in;
    return out.str();
}

}  // namespace magics

#endif
