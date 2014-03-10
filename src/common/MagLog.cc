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

/*! \file MagLog.cc
    \brief Implementation of the MagLog class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

    Changes:
*/

#include <algorithm>
#include <MagLog.h>
#include "MagicsObserver.h"

using namespace magics;

MagLog  MagLog::log_;
bool MagLog::header_;

class MagLogObserver : public ostringstream
{
public:
	MagLogObserver() {}
	~MagLogObserver() {}
	
	void broadcast()
	{
		std::cout << "BROADCAST---->" << str() << "<---BROADCAST----";
		flush();
	}
};

static MagLogObserver MYLOG;


//extern string lowerCase(const string&);

inline bool setMsg(const string& name, bool def)
{
	string value = getEnvVariable(name);
	//value = lowerCase(value);
	if (value == "no" || value == "off" || value == "false") return false;
	if (value == "yes"|| value == "on"  || value == "true")  return true;

	return def;
}

/*!
 \todo Decide if Warnings are plotted when MAGPLUS_QUIET 
*/
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
    warnings_(0)
{
    debug_ = setMsg("MAGPLUS_DEBUG", false);
    dev_ = setMsg("MAGPLUS_DEV", false);
    info_ = setMsg("MAGPLUS_INFO", false);
    profiling_ = setMsg("MAGPLUS_PROFILE", false);
    if(setMsg("MAGPLUS_QUIET",false) )
    {
       info_      = false;
       dev_       = false;
       profiling_ = false;
       debug_    = false;
       userInfo_ = false;
    }
    header_ = true;
}

MagLog::~MagLog() 
{
	//if ( reporter_ ) delete reporter_;
}

static void niceprint(int nb, const string& legend, const string& sep, ostream& out)
{
	if (!nb) return;
	string plurial = (nb > 1) ? "s" : "";
	out << sep << nb << legend << plurial; 
}
	
void ErrorReporter::report(ostream& out)  const
{
	if ( !warnings_ &&  !errors_) return;

	out << " - [ ";
	niceprint(warnings_, " warning", "", out);
	string sep = (warnings_) ? ", " : "";
	niceprint(errors_, " error", sep, out);
	out << " ]";
	warnings_ = 0;
	errors_   = 0;
}


void MagLog::print(ostream&)  const
{
}


ostream& MagLog::warning()
{
	if ( log_.reporter_ ) log_.reporter_->warning();
	// Here we broadcast some eventuel infos...
	broadcast();
	if (log_.warning_)
	{
		if ( log_.warnings_ ++ == 100 ) {
			log_.warningstream_ << "Magics-warning: Too many warnings! Stop sending them ..." << endl;
			return log_.devnull_;
		}

		if  ( log_.warnings_  < 100 ) {
			if ( log_.observers_.empty() ) {
				std::cout  << "Magics-warning: ";
				return cout;
			}
			log_.warningstream_ << "Magics-warning: ";
			return log_.warningstream_;
		}
	}
	return log_.devnull_;
}

ostream& MagLog::error()
{
	if ( log_.reporter_ ) log_.reporter_->error();
	broadcast();
	if (log_.error_)
	{
		if ( log_.observers_.empty() ) {
			std::cout  << "Magics-ERROR: ";
			return cout;
		}
		log_.errorstream_ << "Magics-ERROR: ";
		return log_.errorstream_;
	}
	return log_.devnull_;
}


ostream& MagLog::debug()
{
	if (log_.debug_)
	{
		string text = ( header_ )  ? "Magics-debug: " : "";
		std::cout << text;
		return cout;
	}
	return log_.devnull_;
}

ostream& MagLog::profile()
{
	if (log_.profiling_)
	{
		string text = ( header_ )  ? "Magics-profile: " : "";
		std::cout << text;
		return cout;
	}
	return log_.devnull_;
}

ostream& MagLog::dev()
{
	if (log_.dev_) {
		string text = ( header_ )  ? "Magics-dev: " : "";
		std::cout << text;		
		return cout;
	}
	return log_.devnull_;
}

ostream& MagLog::info()
{
	// Here we broadcast some eventuel infos...
	broadcast();
	if (log_.info_)
	{
		if ( log_.observers_.empty() ) {
			std::cout  << "Magics-warning: ";
			return cout;
		}
		log_.infostream_ << "Magics-info: ";
		return log_.infostream_;
	}
	return log_.devnull_;
}

ostream& MagLog::progress()
{
	// Here we broadcast some eventuel infos...
	broadcast();

	log_.progressstream_ << "Magics-progress: ";
	return log_.progressstream_;
}

void MagLog::progress(const string& msg)
{
	for(vector<MagicsObserver*>::iterator observer = log_.observers_.begin(); observer != log_.observers_.end(); ++observer) 
	{			
		(*observer)->progressMessage(msg);
	}
}

ostream& MagLog::userInfo()
{
	if (log_.userInfo_)
	{
		std::cout << "Magics :";
		return cout;
	}
	return log_.devnull_;
}


void MagLog::broadcast()
{
	for ( vector<MagicsObserver*>::iterator observer = log_.observers_.begin(); observer != log_.observers_.end(); ++observer) {
		if ( !log_.warningstream_.str().empty() ) {
			(*observer)->warningMessage(log_.warningstream_.str());
		}
		if ( !log_.errorstream_.str().empty() ) {
			(*observer)->errorMessage(log_.errorstream_.str());
		}		
		if ( !log_.infostream_.str().empty() ){
			(*observer)->infoMessage(log_.infostream_.str());
		}
		if ( !log_.progressstream_.str().empty() ){
			(*observer)->progressMessage(log_.progressstream_.str());
		}
	}
	log_.warningstream_.str("");
	log_.errorstream_.str("");
	log_.infostream_.str("");
	log_.progressstream_.str("");
}

ostream& MagLog::fatal()
{
	if (log_.fatal_)
	{
		std::cout << "Magics-fatal: ";
		return cout;
	}
	return log_.devnull_;
}

void MagLog::unregisterObserver(MagicsObserver* observer)
{
	 vector<MagicsObserver*>::iterator o = std::find(log_.observers_.begin(), log_.observers_.end(), observer);
	 if ( o != log_.observers_.end() )
		 log_.observers_.erase(o);
}
