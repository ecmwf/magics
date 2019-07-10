/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagLog.cc
    \brief Implementation of the MagLog class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

    Changes:
*/

#include <MagLog.h>
#include <algorithm>
#include "MagicsObserver.h"

using namespace magics;

MagLog MagLog::log_;
bool MagLog::header_;

namespace magics {
class MagLogObserver : public ostringstream {
public:
    MagLogObserver() : silent_(false) {}
    ~MagLogObserver() {}

    virtual void broadcast(const string& msg) {
        std::cout << "BROADCAST---->" << msg << "<---BROADCAST----";
        flush();
    }
    bool silent_;
    void silent() { silent_ = true; }
    virtual void warning(const string& msg) {}
    virtual void error(const string& msg) {}
    virtual void info(const string& msg) {}
    virtual void debug(const string& msg) {}
};

typedef void (*LOG)(void*, const char*);

class WarningLogObserver : public MagLogObserver {
public:
    WarningLogObserver(void* data, LOG cb) : data_(data), callback_(cb) {}
    ~WarningLogObserver() { (*callback_)(data_, "\n"); }

    void warning(const string& msg) {
        if (silent_)
            return;
        if (msg.size())
            (*callback_)(data_, msg.c_str());
    }
    void* data_;
    LOG callback_;
};

class ErrorLogObserver : public MagLogObserver {
public:
    ErrorLogObserver(void* data, LOG cb) : data_(data), callback_(cb) {}
    ~ErrorLogObserver() {}

    void error(const string& msg) {
        if (silent_)
            return;
        if (msg.size())
            (*callback_)(data_, msg.c_str());
    }
    void* data_;
    LOG callback_;
};

class InfoLogObserver : public MagLogObserver {
public:
    InfoLogObserver(void* data, LOG cb) : data_(data), callback_(cb) {}
    ~InfoLogObserver() {}

    void info(const string& msg) {
        if (silent_)
            return;
        if (msg.size())
            (*callback_)(data_, msg.c_str());
    }
    void* data_;
    LOG callback_;
};
class DebugLogObserver : public MagLogObserver {
public:
    DebugLogObserver(void* data, LOG cb) : data_(data), callback_(cb) {}
    ~DebugLogObserver() {}

    void debug(const string& msg) {
        if (silent_)
            return;
        if (msg.size())
            (*callback_)(data_, msg.c_str());
    }
    void* data_;
    LOG callback_;
};

}  // namespace magics

static MagLogObserver MYLOG;


// extern string lowerCase(const string&);

inline bool setMsg(const string& name, bool def) {
    string value = getEnvVariable(name);
    // value = lowerCase(value);
    if (value == "no" || value == "off" || value == "false")
        return false;
    if (value == "yes" || value == "on" || value == "true")
        return true;

    return def;
}

/*!
 \todo Decide if Warnings are plotted when MAGPLUS_QUIET
*/


void output(void*, const char* msg) {
    cout << msg;
}


MagLog::MagLog() :
    reporter_(0),
    devnull_("/dev/null"),
    debug_(true),
    dev_(true),
    info_(true),
    userInfo_(true),
    warning_(true),
    error_(true),
    fatal_(true),
    profiling_(true),
    warnings_(0) {
    debug_     = setMsg("MAGPLUS_DEBUG", false);
    dev_       = setMsg("MAGPLUS_DEV", false);
    info_      = setMsg("MAGPLUS_INFO", false);
    profiling_ = setMsg("MAGPLUS_PROFILE", false);
    if (setMsg("MAGPLUS_QUIET", false)) {
        info_      = false;
        dev_       = false;
        profiling_ = false;
        debug_     = false;
        userInfo_  = false;
    }


    // Not listening to the errors anymore
    /*
    defaultWarning_ = new WarningLogObserver(NULL, output);
    defaultError_   = new ErrorLogObserver(NULL, output);

    defaultInfo_  = new InfoLogObserver(NULL, output);
    defaultDebug_ = new DebugLogObserver(NULL, output);
    if (!info_)
        defaultInfo_->silent();
    if (!dev_)
        defaultDebug_->silent();

    // listeners_.push_back(defaultWarning_);
    //
    // listeners_.push_back(defaultError_);
    // listeners_.push_back(defaultDebug_);
    // listeners_.push_back(defaultInfo_);
    */
    header_ = true;
}

MagLog::~MagLog() {}


static void niceprint(int nb, const string& legend, const string& sep, ostream& out) {
    if (!nb)
        return;
    string plurial = (nb > 1) ? "s" : "";
    out << sep << nb << legend << plurial;
}

void ErrorReporter::report(ostream& out) const {
    if (!warnings_ && !errors_)
        return;

    out << " - [ ";
    niceprint(warnings_, " warning", "", out);
    string sep = (warnings_) ? ", " : "";
    niceprint(errors_, " error", sep, out);
    out << " ]";
    warnings_ = 0;
    errors_   = 0;
}


void MagLog::print(ostream&) const {}

void MagLog::addWarningListener(void* data, LOG cb) {
    log_.listeners_.push_back(new WarningLogObserver(data, cb));
    // log_.defaultWarning_->silent();
}

void MagLog::addErrorListener(void* data, LOG cb) {
    log_.listeners_.push_back(new ErrorLogObserver(data, cb));
    // log_.defaultError_->silent();
}

void MagLog::addInfoListener(void* data, LOG cb) {
    log_.listeners_.push_back(new InfoLogObserver(data, cb));
    // log_.defaultInfo_->silent();
}

void MagLog::addDebugListener(void* data, LOG cb) {
    log_.listeners_.push_back(new DebugLogObserver(data, cb));
    // log_.defaultDebug_->silent();
}

void MagLog::clearListeners() {
    // Here we should delete the listeners
    log_.listeners_.clear();
}

ostream& MagLog::warning() {
    if (log_.reporter_)
        log_.reporter_->warning();
    // Here we broadcast some eventuel infos...
    broadcast();

    if (log_.warning_) {
        if (log_.warnings_++ > 10) {
            // log_.warningstream_ << "Magics-warning: Too many warnings! Stop sending them ..." << endl;
            return log_.devnull_;
        }
        else {
            log_.warningstream_ << "Magics-warning: ";
            return log_.warningstream_;
        }
    }

    return log_.devnull_;
}

ostream& MagLog::error() {
    if (log_.reporter_)
        log_.reporter_->error();
    broadcast();
    if (log_.error_) {
        log_.errorstream_ << "Magics-ERROR: ";
        return log_.errorstream_;
    }
    return log_.devnull_;
}


ostream& MagLog::debug() {
    broadcast();
    log_.debugstream_ << "Magics-debug: ";
    return log_.debugstream_;
}

ostream& MagLog::profile() {
    if (log_.profiling_) {
        string text = (header_) ? "Magics-profile: " : "";
        std::cout << text;
        return cout;
    }
    return log_.devnull_;
}

ostream& MagLog::dev() {
    broadcast();
    log_.debugstream_ << "Magics-dev: ";
    return log_.debugstream_;
}

ostream& MagLog::info() {
    // Here we broadcast some eventuel infos...
    broadcast();
    if (log_.info_) {
        log_.infostream_ << "Magics-info: ";
        return log_.infostream_;
    }
    return log_.devnull_;
}

ostream& MagLog::progress() {
    // Here we broadcast some eventuel infos...
    broadcast();

    log_.progressstream_ << "Magics-progress: ";
    return log_.progressstream_;
}

void MagLog::progress(const string& msg) {
    for (vector<MagicsObserver*>::iterator observer = log_.observers_.begin(); observer != log_.observers_.end();
         ++observer) {
        (*observer)->progressMessage(msg);
    }
}

ostream& MagLog::userInfo() {
    broadcast();
    if (log_.userInfo_) {
        cout << "Magics:";
        return cout;
    }
    return log_.devnull_;
}


void MagLog::broadcast() {
    // disable broadcast

    bool more = true;
    for (vector<MagLogObserver*>::iterator observer = log_.listeners_.begin(); observer != log_.listeners_.end();
         ++observer) {
        (*observer)->warning(log_.warningstream_.str());
        (*observer)->error(log_.errorstream_.str());
        (*observer)->info(log_.infostream_.str());
        (*observer)->debug(log_.debugstream_.str());
        more = false;
    }

    if (more)
        for (vector<MagicsObserver*>::iterator observer = log_.observers_.begin(); observer != log_.observers_.end();
             ++observer) {
            if (!log_.warningstream_.str().empty()) {
                (*observer)->warningMessage(log_.warningstream_.str());
            }
            if (!log_.errorstream_.str().empty()) {
                (*observer)->errorMessage(log_.errorstream_.str());
            }
            if (!log_.infostream_.str().empty()) {
                (*observer)->infoMessage(log_.infostream_.str());
            }
            if (!log_.progressstream_.str().empty()) {
                (*observer)->progressMessage(log_.progressstream_.str());
            }
        }
    log_.warningstream_.str("");
    log_.errorstream_.str("");
    log_.infostream_.str("");
    log_.progressstream_.str("");
}

ostream& MagLog::fatal() {
    if (log_.fatal_) {
        std::cout << "Magics-fatal: ";
        return cout;
    }
    return log_.devnull_;
}

void MagLog::unregisterObserver(MagicsObserver* observer) {
    vector<MagicsObserver*>::iterator o = std::find(log_.observers_.begin(), log_.observers_.end(), observer);
    if (o != log_.observers_.end())
        log_.observers_.erase(o);
}
