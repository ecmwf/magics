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

#include "magics.h"
#include <fstream>
using std::ofstream;

namespace magics {


class ErrorReporter
{
public:
	ErrorReporter() : errors_(0), warnings_(0) {}
	void error()  const { errors_++; }
	void warning() const { warnings_++; }
	void report(ostream&) const;

protected:
	mutable int errors_;
	mutable int warnings_;
};

class MagicsObserver;

class MagLog {

public:
	MagLog();
	//! Destructor
	~MagLog();
	
	

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
	
	static void broadcast();

	static void devMessage(bool dev = true)
		 { log_.dev_ = dev; }
	
	static void infoMessage(bool info = true)
		 { log_.info_ = info; }
	
	static void userInfoMessage(bool info = true)
		 { log_.userInfo_ = info; }

	static void warningMessage(bool warning = true)
		 { log_.warning_ = warning; }

	static void debugMessage(bool debug = true)
		 { log_.debug_ = debug; }

	static void errorMessage(bool error = true)
		 { log_.error_ = error; }
	
	static void profilingMessage(bool error = true)
			 { log_.error_ = error; }

	static void fatalMessage(bool fatal = true)
		 { log_.fatal_ = fatal; }

	static void header(bool header)
		 { header_ = header; }

	static void setReporter(const ErrorReporter* reporter) { log_.reporter_ = reporter; }
	static void registerObserver(MagicsObserver* observer) { log_.observers_.push_back(observer); }
	static void unregisterObserver(MagicsObserver* observer);

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

	string stamp_;
	
	int warnings_;
	
	ostringstream infostream_;
	ostringstream warningstream_;
	ostringstream errorstream_;
	ostringstream progressstream_;
	vector<MagicsObserver*> observers_;

// -- Friends
	friend ostream& operator<<(ostream& s,const MagLog& p)
		{ p.print(s); return s; }
};

#define LOGDEV(a) MagLog::dev() << __FUNCTION__ << "--->" << #a << " = " << a << "\n";
#define VALGRIND  MagLog::dev() << "valgrind----->" << __FUNCTION__ << "\n";
}
#endif
