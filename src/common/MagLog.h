/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagLog.h
    \brief Definition of log class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

*/

#ifndef MPPMagLog_H
#define MPPMagLog_H

#include <fstream>
#include "magics.h"
using std::ofstream;

namespace magics {


class ErrorReporter {
public:
    ErrorReporter() : errors_(0), warnings_(0) {}
    void error() const { errors_++; }
    void warning() const { warnings_++; }
    void report(ostream&) const;

protected:
    mutable int errors_;
    mutable int warnings_;
};

class MagicsObserver;
class MagLogObserver;

typedef void (*LogListener)(void*, char*);

class MagLog {
public:
    MagLog();
    //! Destructor
    ~MagLog();

    vector<MagLogObserver*> listeners_;

    MAGICS_EXPORT static void addWarningListener(void*, void (*)(void*, const char*));
    MAGICS_EXPORT static void addErrorListener(void*, void (*)(void*, const char*));
    MAGICS_EXPORT static void addInfoListener(void*, void (*)(void*, const char*));
    MAGICS_EXPORT static void addDebugListener(void*, void (*)(void*, const char*));

    MAGICS_EXPORT static void clearListeners();


    // -- Methods
    MAGICS_EXPORT static ostream& warning();
    MAGICS_EXPORT static ostream& debug();
    MAGICS_EXPORT static ostream& info();
    MAGICS_EXPORT static ostream& userInfo();
    MAGICS_EXPORT static ostream& error();
    MAGICS_EXPORT static ostream& fatal();
    MAGICS_EXPORT static ostream& dev();
    MAGICS_EXPORT static ostream& profile();
    MAGICS_EXPORT static ostream& progress();
    MAGICS_EXPORT static void progress(const string&);

    MAGICS_EXPORT static void broadcast();

    MAGICS_EXPORT static void devMessage(bool dev = true);

    MAGICS_EXPORT static void infoMessage(bool info = true);

    MAGICS_EXPORT static void userInfoMessage(bool info = true);

    MAGICS_EXPORT static void warningMessage(bool warning = true);

    MAGICS_EXPORT static void debugMessage(bool debug = true);

    MAGICS_EXPORT static void errorMessage(bool error = true);

    MAGICS_EXPORT static void profilingMessage(bool error = true);

    MAGICS_EXPORT static void fatalMessage(bool fatal = true);

    MAGICS_EXPORT static void header(bool header);

    MAGICS_EXPORT static void setReporter(const ErrorReporter* reporter);
    MAGICS_EXPORT static void registerObserver(MagicsObserver* observer);
    MAGICS_EXPORT static void unregisterObserver(MagicsObserver* observer);

protected:
    // -- Methods
    void print(ostream&) const;
    const ErrorReporter* reporter_;

private:
    // No copy allowed
    MagLog(const MagLog&);
    MagLog& operator=(const MagLog&);

    // -- Members
    static MagLog log_;
    static bool header_;

    ofstream devnull_;
    bool debug_;
    bool dev_;
    bool info_;
    bool userInfo_;
    bool warning_;
    bool error_;
    bool fatal_;
    bool profiling_;

    MagLogObserver* defaultWarning_;
    MagLogObserver* defaultInfo_;
    MagLogObserver* defaultDebug_;
    MagLogObserver* defaultError_;


    string stamp_;

    int warnings_;

    ostringstream infostream_;
    ostringstream warningstream_;
    ostringstream errorstream_;
    ostringstream debugstream_;
    ostringstream progressstream_;
    vector<MagicsObserver*> observers_;

    // -- Friends
    friend ostream& operator<<(ostream& s, const MagLog& p) {
        p.print(s);
        return s;
    }
};

#define LOGDEV(a) MagLog::dev() << __FUNCTION__ << "--->" << #a << " = " << a << "\n";
#define VALGRIND MagLog::dev() << "valgrind----->" << __FUNCTION__ << "\n";
}  // namespace magics
#endif
