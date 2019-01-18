/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MetaData.h
    \brief Implementation of the Template class MetaData.

    Magics Team - ECMWF 2006

    Started: Thu 5-Jan-2006

    Changes:

*/

#include "MetaData.h"
#include "Timer.h"

#ifndef MAGICS_ON_WINDOWS
#include <sys/time.h>
#include <sys/resource.h>
#endif


using namespace magics;

MetaDataVisitor::MetaDataVisitor() 
{
	meta_.push_back(this);
}

string MetaDataVisitor::start_;

MetaDataVisitor::~MetaDataVisitor() 
{
	meta_.erase(std::remove(meta_.begin(),meta_.end(), this), meta_.end());
}

/*!
 Class information are given to the output-stream.
*/		
void MetaDataVisitor::print(ostream& out)  const
{
	out << "MetaData[";
	out << "]";
}

string now()
{
	char tmp[256];
	timeval t;
	gettimeofday(&t,0);
	sprintf(tmp, "%f", (t.tv_sec + t.tv_usec / 1000000.0));
	return string(tmp);

}
void MetaDataVisitor::start()
{
	start_ = now();
}

void MetaDataVisitor::collectMetaData()
{
	MagLog::dev() << "----MetaData::visit-----" << endl;
	parent_->visit(*this);
	
	try {
		ofstream out(profile_.c_str());

		out << "{\n\"timers\" : {" << endl;

		string s = "";
		for (vector<ProfileInfo>::const_iterator web = Timer::begin(); web != Timer::end(); ++web) {
			out << s;
			out << *web;
			s = ",\n";
		}
#ifndef MAGICS_ON_WINDOWS // windows doesn't support rusage
		struct rusage p;
		getrusage(RUSAGE_SELF, &p);
		out << "\n\t},\n\t\"start\": " << start_;
		out << ",\n\t\"stop\": " << now();
		out << ",\n\t\"general\": {" << endl;
		out << "\t\t\"user\" : " <<  p.ru_utime.tv_sec << "," << endl;
		out << "\t\t\"system\" : " <<  p.ru_stime.tv_sec << "," << endl;
		out << "\t\t\"memory\" : " <<   p.ru_maxrss/1024 << ","   << endl;
		out << "\t\t\"input\" : " <<   p.ru_inblock  << "," << endl;
		out << "\t\t\"output\" : " <<   p.ru_oublock  << endl;
		out << "\t}" << endl;
		out << "}" << endl;
#endif
	}

	catch ( ...) {}

	if ( !wms_file_.empty() && styles_.size() ) {
	try  {
			ofstream out(wms_file_);

			
		
			

			for (auto &style : styles_)
				out << *style;
			
			out.close();
		}
		catch (...) {}
	}
	if ( ! javascript_.empty() ) {
		try  {
			ofstream out(javascript_.c_str());

			out        << "{";
		
			string s = "";

			for (map<string, string>::const_iterator web = web_.begin(); web != web_.end(); ++web) {
				if ( web->first == "world_file" )
					continue;
				out << s;
				out << "\"" << web->first << "\":" <<  web->second;
				s = ",";
			}

			out << "}";
			out.close();
		}
		catch (...) {}
	}
	
	if ( ! world_file_.empty() ) {
		try  {
			ofstream out(world_file_.c_str());

			for (map<string, string>::const_iterator web = web_.begin(); web != web_.end(); ++web) {
				if ( web->first != "world_file" )
					continue;

				out << web->second;

			}

			out.close();
		}
		catch (...) {}
	}
	if ( ! efi_.empty() ) {
				char c;
				string path = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "efi/" + efi_template_;
				try  {
					ofstream out(efi_.c_str());
					ifstream in(path.c_str());
					while(in && in.get(c) )
					    out.put(c);
					out.close();
					in.close();
				}
				catch (...) {
					ofstream out(efi_.c_str());
					out << " Could not opened " << path << endl;
					out.close();
				}
	}
}

void MetaDataVisitor::metadata(map<string, string>& data)
{
    string quote = "\"";
	data["magics_version"] = quote + MAGICS_VERSION + quote;
    data["filename"] = javascript_;
    parent_->visit(*this);
}

vector<MetaDataVisitor*> MetaDataVisitor::meta_;

void MetaDataVisitor::collect() 
{
	for ( vector<MetaDataVisitor*>::iterator meta = meta_.begin(); meta != meta_.end(); ++meta ) 
		(*meta)->collectMetaData();
}

#include "ContourLibrary.h"
void StyleEntry::print(ostream& s) const
{
	StyleLibrary styles = *WebLibrary::styles_;

	s << "{\"styles\": [ " << endl;
	string sep = "    ";
	for (auto style = styles_.begin(); style != styles_.end(); ++style) {
		string description = styles.getAttribute(*style, "contour_description", "description to come");
		string title = styles.getAttribute(*style, "contour_title", "title to come");
		s << sep << "{\"name\":\"" << *style << "\"";
		sep = ",\n      ";
		s << sep << "\"description\":\"" << description << "\"";
		s << sep << "\"title\":\"" << title << "\"";
		s << sep << "\"legend\": { \"width\": 350"; 
		sep = ",\n          ";
		s << sep << "\"height\": 50 }";
		sep = "\n    ";
		s << sep << "}";
		sep = ",\n    ";
	}
	s << " 	  ]" << endl;
	s << "  }" << endl;
}
