/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MagConfig.h"
#include "MagLog.h"
#include "MagExceptions.h"

using namespace magics;
using namespace json_spirit;


MagConfigHandler::MagConfigHandler(const string& config, MagConfig& magics)
{
	ifstream is(config.c_str());
	if ( !is.good() ) {
		MagLog::error() << "Could not processed find the file: " << config << endl;
		return;
	} 

	json_spirit::Value value;
	try {
		 json_spirit::read_or_throw( is, value );
		 Object object = value.get_value< Object >();

		 for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
			 magics.callback(entry->name_, entry->value_);
		   }
	}
	catch (std::exception e) {
		  MagLog::error() << "Could not processed the file: " << config << ": " << e.what() << endl;
	}
}

MagConfigHandler::~MagConfigHandler()
{
	
}

void MagConfigHandler::dig(const json_spirit::Value&)
{

}

void MagConfigHandler::print(ostream& out) const
{
	out << "MagConfigHandler[";
	out << "]";
}


MagConfig::MagConfig()
{

}

MagConfig::~MagConfig()
{

}


string MagConfig::convert(const json_spirit::Value& value)
{

	if (value.type() == str_type) {
		return value.get_str();

	}
	if (value.type() == int_type) {
		return tostring(value.get_int());

	}
	if (value.type() == real_type) {
		return tostring(value.get_real());
	}

	return "";


}

void StyleLibrary::callback(const string& name, const json_spirit::Value& value)
{
		library_.insert(make_pair(name, map<string, string>()));
		if ( value.type() == json_spirit::obj_type ) {
			json_spirit::Object object =value.get_value< json_spirit::Object >();
			for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
				library_[name].insert(make_pair(entry->name_, convert(entry->value_)));
    		}
    	}
    	
}
void StyleLibrary::init()
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + theme_ + "/" + family_ +".json";
	MagLog::debug() << "Opening " << library << endl;
	MagConfigHandler(library,  *this);
}



const map<string, string>& StyleLibrary::get(const string& name) const {
		map<string, map<string, string> >::const_iterator area = library_.find(name);
		if ( area != library_.end() ) 
			return area->second;
		if (theme_.empty() )
			MagLog::warning() << "Could not find the style " << name << " for " << family_ << endl;
		else 
			MagLog::warning() << "Could not find the style " << name << " for " << family_ << " in " << theme_ << endl;

		return empty_;
}