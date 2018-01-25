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

void StyleLibrary::callback(const json_spirit::Array& values)
{
	for (unsigned int i = 0; i < values.size(); i++) {
		library_.push_back(Style());

		json_spirit::Object object = values[i].get_value< json_spirit::Object >();
		library_.back().set(object);
		
	}
		
    	
}

void Style::criteria(const json_spirit::Value& value) 
{
	json_spirit::Object object =value.get_value< json_spirit::Object >();
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		criteria_.insert(make_pair(entry->name_, MagConfig::convert(entry->value_)));
	}
}
void Style::style(const json_spirit::Value& value) 
{
	json_spirit::Object object =value.get_value< json_spirit::Object >();
	for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
		style_.insert(make_pair(entry->name_, MagConfig::convert(entry->value_)));
	}
}
void Style::more(const json_spirit::Value& value) 
{
	Array values = value.get_value<Array>();
	cout << "Style::more-->" << values.size() << endl;
	for (unsigned int i = 0; i < values.size(); i++) {
		more_.push_back(Style());

		json_spirit::Object object = values[i].get_value< json_spirit::Object >();
		more_.back().set(object);
		
	}

}
void Style::set(const json_spirit::Object& object) 
{
	if ( methods_.empty() ) {
		methods_["criteria"] =  &Style::criteria;
		methods_["criterias"] =  &Style::criteria;
		methods_["style"] =  &Style::style;
		methods_["visdef"] =  &Style::style;
		methods_["more"] =  &Style::more;
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
		/*
		library_.insert(make_pair(name, map<string, string>()));
		if ( value.type() == json_spirit::obj_type ) {
			json_spirit::Object object =value.get_value< json_spirit::Object >();
			for (vector<json_spirit::Pair>::const_iterator entry = object.begin(); entry !=  object.end(); ++entry) {
				library_[name].insert(make_pair(entry->name_, convert(entry->value_)));
    		}
    	}
    	*/
    	
}
void StyleLibrary::init()
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + theme_ + "/" + family_ +".json";
	MagLog::debug() << "Opening " << library << endl;
	MagConfigHandler(library,  *this);
}

void PaletteLibrary::init()
{
	string library = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_  + "/" + "palette" +".json";
	MagLog::debug() << "Opening " << library << endl;
	MagConfigHandler(library,  *this);
}
void Palette::values(const json_spirit::Value& value) 
{
	Array values = value.get_value<Array>();
	cout << "Palette::values-->" << values.size() << endl;
	for (unsigned int i = 0; i < values.size(); i++) {
		colours_.push_back(MagConfig::convert(values[i]));	
	}
}

void Palette::tags(const json_spirit::Value& value) 
{
	Array values = value.get_value<Array>();
	cout << "Palette::values-->" << values.size() << endl;
	for (unsigned int i = 0; i < values.size(); i++) {
		tags_.push_back(MagConfig::convert(values[i]));	
	}

}

void Palette::set(const json_spirit::Object& object)
{
	if ( methods_.empty() ) {
		methods_["values"] =  &Palette::values;
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



bool Style::find(const Definition& data, Definition& visdef)
{
	for (Definition::const_iterator value = data.begin(); value != data.end(); ++value) {
		cout << "Criteria-->" << value->first << endl;
 		Definition::iterator criteria = criteria_.find(value->first);
		if ( criteria != criteria_.end() && criteria->second == value->second ) {
			visdef = style_;
			for ( vector<Style>::iterator other = more_.begin(); other != more_.end(); ++other) 
				if ( other->find(data, visdef) )
					return true;
			return true;
		}
	}
	return false;

}

bool StyleLibrary::find(const Style::Definition& data, Style::Definition& visdef)
{
	for (vector<Style>::iterator style = library_.begin(); style != library_.end(); ++style)
		if ( style->find(data, visdef) ) {
			return true;
		}

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

