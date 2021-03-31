/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file System.h
    \brief In this file wrappers are defined for system calls
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

*/
#ifndef SystemInfo_H
#define SystemInfo_H

#include <magics.h>
#include <cstdlib>
#ifndef MAGICS_ON_WINDOWS
#include <pwd.h>
#include <unistd.h>
#else
#include <lmcons.h>
#include "win_time.h"
#endif

namespace magics {

/*! \class SystemInfo
    \brief This class gives information about the system.

     This class is very much platform dependent!!!.

     \todo move this file in a more central directory?
*/
class SystemInfo {
public:
    SystemInfo(){};
    ~SystemInfo(){};  // not virtual - so do not inherit!

    MAGICS_NO_EXPORT string getHostName() const {
        char host[256];
        if (gethostname(host, sizeof(host)) == -1)
            strcpy(host, "unknown-host");
        string tmp(host);
        return tmp;
    };  // end getHostName()

    MAGICS_NO_EXPORT string getTime() const {
        time_t when;
        time(&when);
        string tmp(ctime(&when));
        tmp.erase(25);  // need to cut \n\0
        tmp.erase(24);
        return tmp;
    };

    MAGICS_NO_EXPORT string getUserID() const {
#ifndef MAGICS_ON_WINDOWS
        struct passwd* who = getpwuid(getuid());
        string tmp(who->pw_name);
#else
        TCHAR username[UNLEN + 1];
        DWORD size = UNLEN + 1;
        GetUserName((TCHAR*)username, &size);
        string tmp(username);
#endif
        return tmp;
    };

private:
    // No copy allowed
    SystemInfo(const SystemInfo&);
    SystemInfo& operator=(const SystemInfo&);
};

}  // namespace magics
#endif
