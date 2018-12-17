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
#include "MetaData.h"
#include "magics_windef.h"

#ifndef MAGICS_ON_WINDOWS
	#include <dirent.h>
#else
	#include <direct.h>
	#include <io.h>
#endif

#include <cstring>
#include <Tokenizer.h>

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
		 if (value.type() == array_type) {
		 	Array values = value.get_value<Array>();
		 	magics.callback(values);
		 	return;
		 }
		 Object object = value.get_value< Object >();

		 for (vector<Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
			 magics.callback(entry->name_, entry->value_);
		   }
	}
	catch (json_spirit::Error_position e) {
		  MagLog::error() << "JSON error in file: " << config << ": " << e.reason_ << "[line: " << e.line_ << ", column: " << e.column_ << "]" << endl;
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

void StyleLibrary::callback(const json_spirit::Array& values)
{
	for (unsigned int i = 0; i < values.size(); i++) {
		library_.push_back(Style());
		json_spirit::Object object = values[i].get_value< json_spirit::Object >();
		library_.back().set(object);
	}
}

void Style::set(json_spirit::Object& object, Style::Match& match)
{
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		match.insert(make_pair(entry->name_, vector<string>()));
		if ( entry->value_.type() == array_type ) {
			Array values = entry->value_.get_value<Array>();
			for (unsigned int i = 0; i < values.size(); i++) {
				match[entry->name_].push_back(MagConfig::convert(values[i]));
			}
		}
		else {
			match[entry->name_].push_back(MagConfig::convert(entry->value_));
		}
	}
}

void Style::criteria(const json_spirit::Value& value)
{
	// List of criteria
	Array values = value.get_value<Array>();

	for (unsigned int i = 0; i < values.size(); i++) {
		json_spirit::Object object = values[i].get_value< json_spirit::Object >();
		criteria_.push_back(Style::Match());
		set(object, criteria_.back());
	}
}
void Style::style(const json_spirit::Value& value)
{

}
void Style::name(const json_spirit::Value& value)
{

}
void Style::styles(const json_spirit::Value& value)
{
	Array values = value.get_value<Array>();

	for (unsigned int i = 0; i < values.size(); i++) {
		// If we find a name get it from the library
		if (values[i].type() == obj_type) {
			MagDef def;
			def.set(values[i].get_value<Object>());
			//push to the library !
		}
		else
			styles_.push_back(values[i].get_str());
	}
}
void Style::units(const json_spirit::Value& value)
{
	preferedUnits_ = value.get_str();
}

void Style::match(const json_spirit::Value& value)
{
	json_spirit::Object object =value.get_value< json_spirit::Object >();

	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		map<string,  SetMethod>::iterator method = methods_.find(entry->name_);
		if ( method != methods_.end() )
			(this->*method->second)(entry->value_);
		else
			MagLog::warning() << entry->name_ << " is not a known keyword" << endl;
    }
}

void Style::set(const json_spirit::Object& object)
{
	if ( methods_.empty() ) {
		methods_["match"] =  &Style::criteria;
		methods_["prefered_units"] =  &Style::units;
		methods_["styles"] =  &Style::styles;
		methods_["eccharts_layer"] =  &Style::name;
		methods_["visdef"] =  &Style::style;
		methods_["scaling"] =  &Style::ignore;
	}

	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		map<string,  SetMethod>::iterator method = methods_.find(entry->name_);
		if ( method != methods_.end() )
			(this->*method->second)(entry->value_);
		else
			MagLog::warning() << entry->name_ << " is not a known keyword" << endl;
	}
}

void StyleLibrary::callback(const string& name, const json_spirit::Value& value)
{
		if ( name == "match" ) {
			library_.push_back(Style());
			json_spirit::Object object = value.get_value< json_spirit::Object >();
			library_.back().set(object);
		}
}


void StyleLibrary::init()
{
	// Now we have a variable

	string ecmwf = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "/styles/ecmwf";
	string library = getEnvVariable("MAGICS_STYLE_PATH");

	if (library.empty())
		library = "ecmwf";

	Tokenizer tokenizer(":");
	vector<string> paths;
	tokenizer(library, paths);

	for ( auto token = paths.begin(); token != paths.end(); ++token) {
		string path = magCompare(*token, "ecmwf") ? ecmwf : *token;

#ifndef MAGICS_ON_WINDOWS
		DIR* dir = opendir(path.c_str());
		if ( !dir ) {
			ostringstream error;
			error << "Trying to open directory " << library << ": " << strerror(errno);
			throw FailedSystemCall(error.str());
		}
		struct dirent *entry = readdir(dir);
		while ( entry ) {

			if (entry->d_name[0] != '.') {
				current_ = entry->d_name;
				MagConfigHandler(path + "/" + string(entry->d_name),  *this);
			}

			entry = readdir(dir);
	   }
#else
		struct _finddata_t fileinfo;
		intptr_t handle = _findfirst((path + "/*").c_str(), &fileinfo);
		if (handle == -1) {
			ostringstream error;
			error << "Trying to open directory " << library << ": " << strerror(errno);
			throw FailedSystemCall(error.str());
		} else {
			do {
				if(fileinfo.name[0] != '.') {
					current_ = fileinfo.name;
					MagConfigHandler(path + "/" + string(fileinfo.name), *this);
				}
			} while(!_findnext(handle, &fileinfo));
			_findclose(handle);
		}
#endif
	}

	allStyles_.init(ecmwf, "styles.json");
}

void PaletteLibrary::init()
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_  + "/styles/palettes.json";
	MagConfigHandler(library,  *this);
}


void Palette::values(const json_spirit::Value& value)
{
	Array values = value.get_value<Array>();

	for (unsigned int i = 0; i < values.size(); i++) {
		colours_.push_back(MagConfig::convert(values[i]));
	}
}

void Palette::tags(const json_spirit::Value& value)
{
}

void Palette::set(const json_spirit::Object& object)
{
	if ( methods_.empty() ) {
		methods_["contour_shade_colour_list"] =  &Palette::values;
		methods_["tags"] =  &Palette::tags;
	}
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		map<string,  SetMethod>::iterator method = methods_.find(entry->name_);
		if ( method != methods_.end() )
			(this->*method->second)(entry->value_);
		else
			MagLog::warning() << entry->name_ << " is not a known keyword" << endl;
    }
}
void PaletteLibrary::callback(const string& name, const json_spirit::Value& value)
{
	Palette palette;
	palette.name_ = name;
	json_spirit::Object object = value.get_value< json_spirit::Object >();
	palette.set(object);
	library_.insert(make_pair(name, palette));
}

void UnitsLibrary::init()
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_  + "/units-rules.json";
	MagConfigHandler(library,  *this);
}


void UnitConvert::from(const json_spirit::Value& value)
{
	from_ = value.get_str();
}

void UnitConvert::to(const json_spirit::Value& value)
{
	to_ = value.get_str();
}
void UnitConvert::scaling(const json_spirit::Value& value)
{
	scaling_ = value.get_real();
}

void UnitConvert::offset(const json_spirit::Value& value)
{
	offset_ = value.get_real();
}


void UnitConvert::set(const json_spirit::Object& object)
{
	if ( methods_.empty() ) {
		methods_["from"] =  &UnitConvert::from;
		methods_["to"] =  &UnitConvert::to;
		methods_["scaling"] =  &UnitConvert::scaling;
		methods_["offset"] =  &UnitConvert::offset;
	}
	scaling_ = 1;
	offset_ = 0;
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		map<string,  SetMethod>::iterator method = methods_.find(entry->name_);
		if ( method != methods_.end() )
			(this->*method->second)(entry->value_);
		else
			MagLog::warning() << entry->name_ << " is not a known keyword" << endl;
    }
}
void UnitsLibrary::callback(const string& name, const json_spirit::Value& value)
{
	library_.insert(make_pair(name, vector<UnitConvert>()));
	json_spirit::Array objects = value.get_value< json_spirit::Array >();
	for (unsigned int i = 0; i < objects.size(); i++) {
		UnitConvert convert;
		convert.set(objects[i].get_value<Object>());
		library_[name].push_back(convert);
	}
}


int Style::score(const MetaDataCollector& data)
{
	int bestscore = 0;
	map<string, string> criteria;
	for (auto match = criteria_.begin(); match != criteria_.end(); ++match) {
		int score = 0;
		for (auto key = match->begin(); key != match->end(); ++key) {
			auto dkey= data.find(key->first);

			if ( dkey == data.end() ) {
				continue;
			}
			if ( dkey->second == "" ) {
				continue;
			}
			int tmpscore = 0;
			for ( auto value = key->second.begin(); value != key->second.end(); ++value ) {

				string whitespaces (" \t\f\v\n\r");
				string clean = dkey->second;
  				std::size_t pos = clean.find_last_not_of(whitespaces);
 				if (pos!=std::string::npos) {
    				clean = clean.substr(0, pos+1);
 				}
				if ( *value == clean ) {
					tmpscore++;
					criteria.insert(make_pair(key->first, *value));
					break;
				}
			}
			if ( !tmpscore) {
					criteria.clear();
					score = 0;
					break;
				}
			score++;
		}
		if ( bestscore < score )
			bestscore = score;
	}

	if ( bestscore ) {
		if ( styles_.empty() ) {
			styles_.push_back("default");
		}

		else {
			MagLog::debug() << "----   Found style with score : " << bestscore << " Style --> " << styles_.front() << endl;
			for ( auto match = criteria.begin(); match != criteria.end(); ++match) {
				MagLog::debug() << "    " << match->first  << " == " <<  match->second << endl;
			}
			MagLog::debug() << "----------------------------------------------------" << endl;
		}
	}

	return bestscore;
}

void Style::keywords(std::set<string>& keys)
{
	for (auto match = criteria_.begin(); match != criteria_.end(); ++match) {
		for (auto key = match->begin(); key != match->end(); ++key) {
			keys.insert(key->first);
		}
	}
}

void  StyleLibrary::findStyle(const string& name, MagDef& visdef)
{
	allStyles_.find(name, visdef);
}

void  StyleLibrary::getCriteria(std::set<string>& keys)
{
	for (vector<Style>::iterator style = library_.begin(); style != library_.end(); ++style) {
		style->keywords(keys);
	}
}


bool StyleLibrary::findStyle(const MetaDataCollector& data, MagDef& visdef, StyleEntry& info)
{
	int score = 0;
	Style beststyle;

	for (vector<Style>::iterator style = library_.begin(); style != library_.end(); ++style) {
		int s = style->score(data);
		if ( s > score ) {
			score = s;
			beststyle = *style;
		}
	}
	if ( score ) {
			info.set(beststyle.styles_.front(), beststyle.styles_);
			allStyles_.find(info.default_, visdef);
			if ( visdef.find("prefered_units") == visdef.end() )
				if (beststyle.preferedUnits_.size())
					visdef.insert(make_pair("prefered_units", beststyle.preferedUnits_));
			return true;
		}

	vector<string> empty;
	empty.push_back("default");
	info.set("default", empty);

	allStyles_.find(info.default_, visdef);
	return true;
}

string StyleLibrary::getAttribute(const string& style, const string& param, const string& defval)
{
	MagDef visdef;
	allStyles_.find(style, visdef);
	auto value = visdef.find(param);

	return ( value != visdef.end() ) ? value->second : defval;
}

bool StyleLibrary::findScaling(const MetaDataCollector& data, MagDef& scaling)
{
	return false;
}

void NetcdfGuess::init()
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ +  "/" + name_ +".json";
	MagLog::debug() << "Opening " << library << endl;
	MagConfigHandler(library,  *this);
}

void NetcdfGuess::callback(const string& name, const json_spirit::Value& value)
{
	guess_.insert(make_pair(name, map<string, vector<string> >()));
	if ( value.type() == json_spirit::obj_type ) {
		json_spirit::Object object = value.get_value< json_spirit::Object >();
		for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
			guess_[name].insert(make_pair(entry->name_, vector<string>()));
			json_spirit::Array values = (entry->value_).get_value<json_spirit::Array>();
			for (unsigned int i = 0; i < values.size(); i++) {
				guess_[name][entry->name_].push_back(convert(values[i]));
			}
		}
	}

}
void MagDefLibrary::init(const string& name)
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_   + "/" + name;
	MagLog::dev() << "opening -->" << library << endl;
	MagConfigHandler(library,  *this);
}

void MagDefLibrary::init(const string& path, const string& name)
{
	string library = path  + "/" + name;
	MagLog::dev() << "opening -->" << library << endl;
	MagConfigHandler(library,  *this);
}

void MagDef::values(const json_spirit::Value& value)
{
	json_spirit::Object object =value.get_value< json_spirit::Object >();
}

void MagDef::set(const json_spirit::Object& object)
{
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		insert(make_pair(entry->name_, MagConfig::convert(entry->value_)));
	}
}

void MagDefLibrary::callback(const string& name, const json_spirit::Value& value)
{
	MagDef def;
	def.name_ = name;
	json_spirit::Object object = value.get_value< json_spirit::Object >();
	def.set(object);

	library_.insert(make_pair(name, def));
}
