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
	static ostream& warning();
	static ostream& debug();
	static ostream& info();
	static ostream& userInfo();
	static ostream& error();
	static ostream& fatal();
	static ostream& dev();
	static ostream& profile();
	static ostream& progress();
	static void progress(const string&);
	
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
