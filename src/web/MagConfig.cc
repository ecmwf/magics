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

using namespace magics;
using namespace json_spirit;


MagConfigHandler::MagConfigHandler(const string& config, MagConfig& magics)
{
	ifstream is(config.c_str());

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
		  abort();
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

