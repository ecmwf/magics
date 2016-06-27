/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TagHandler.cc
    \brief Implementation of the Template class TagHandler.
    
    Magics Team - ECMWF 2008
    
    Started: Mon 27-Oct-2008
    
    Changes:
    
*/



#include "TagHandler.h"
#include "XmlReader.h"
using namespace magics;

TagHandler::TagHandler()
{
}


TagHandler::~TagHandler() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void TagHandler::print(ostream& out)  const
{
	out << "TagHandler[";
	for (map<string, DefList >::const_iterator fam = definitions_.begin(); fam != definitions_.end(); ++fam) {
		out << " Famille-> " << fam->first;
		
		DefList defs = fam->second;
		for ( DefList::const_iterator def = defs.begin(); def != defs.end(); ++def) {
			string sep;
			string value;
			for ( std::set<string>::const_iterator v = def->second.begin(); v != def->second.end(); ++v) {
				if ((*v).empty()) continue;
				value += sep + *v; 
				sep ="/";
			}
			out << " (" << def->first << "=" << value <<  ")" << endl;
		}
	}
	out << "]";
}

void TagHandler::update(const string& family, const string& definition, const string& value)
{
	map<string, DefList >::iterator fam = definitions_.find(family);
	if ( fam == definitions_.end()) {
		definitions_.insert(make_pair(family, DefList()) );
		fam = definitions_.find(family);
	}
	DefList::iterator def = fam->second.find(definition);
	
	if ( def == fam->second.end() ) {
		fam->second.insert(make_pair(definition, std::set<string>()));
		def = fam->second.find(definition);
	}

	def->second.insert(value);
}


string TagHandler::get(const string& family, const string& definition)
{
	map<string, DefList >::iterator fam = definitions_.find(family);

	if ( fam == definitions_.end() ) return "";

	DefList::iterator def = fam->second.find(definition);

	if ( def == fam->second.end() ) return "";

	string sep = "";
	string value;
	for ( std::set<string>::const_iterator v = def->second.begin(); v != def->second.end(); ++v) {
		if ((*v).empty()) continue;
		value += sep + *v; 
		sep ="/";
	}
	return value;
}

TagConverter::TagConverter(TagHandler& owner) : owner_(owner), text_(0) {
		map_["font"] =  &TagConverter::font;
		map_["u"] =  &TagConverter::underline;
		map_["b"] =  &TagConverter::bold;
		map_["sup"] =  &TagConverter::superscript;
		map_["sub"] =  &TagConverter::subscript;
		map_["root"] =  &TagConverter::ignore;
		map_["xml"] =  &TagConverter::text;
		map_["data"] =  &TagConverter::data;
		map_["grib_info"] =  &TagConverter::grib;
		map_["json_info"] =  &TagConverter::json;
		map_["spot_info"] =  &TagConverter::spot;
		map_["netcdf_info"] =  &TagConverter::netcdf;
		map_["spot_info"] =  &TagConverter::spot;
		map_["magics_title"] =  &TagConverter::magics;
		map_["base_date"] =  &TagConverter::base_date;
		map_["text"] =  &TagConverter::pass;
		map_["box"] =  &TagConverter::pass;
		map_["br"] =  &TagConverter::pass;
	}
	
TagConverter::~TagConverter() {}

void TagConverter::visit(const XmlNode& node)
{		
	map<string, ConvertFunction>::iterator function = map_.find(node.name());		
	if ( function != map_.end())  (this->*function->second)(node);			
	else {
		MagLog::error() << "Html Text handler: unknown tag [" << node.name() << "]" << endl;
		MagLog::broadcast();
	}
	    
	// continue the visit...
		
}
void TagConverter::ignore(const XmlNode&) {}
void TagConverter::encoding(const string& encoding) { encoding_ = encoding; }
void TagConverter::text(const XmlNode& node) { 
	push();
	node.visit(*this);

}

void TagConverter::grib(const XmlNode& node) { 
	//MagLog::dev()<< "checking grib!" << endl;
	push();
	const map<string, string>& attributes = node.attributes();
	string key =  ( attributes.find("id") != attributes.end() ) ? attributes.find("id")->second : ""; 
	if (attributes.find("key") != attributes.end() ) {
		string result = owner_.get("grib"+key, attributes.find("key")->second);
		if ( result.empty() ) {
			if ( automatic_ == 1 )
				automatic_ = 0;
		}
		else {
			if ( automatic_ == 0 )
				automatic_ = 1;
			automatic_++;
			check(owner_.get("grib"+key, attributes.find("key")->second));
		}
	}
	if (attributes.find("definition") != attributes.end() ) {			
		check(owner_.get("grib"+key, attributes.find("definition")->second)); 
	}
	node.visit(*this);
	pop();	
	
	font_ = top().font();
	elevation_ = top().elevation();
}
/*
void TagConverter::spot(const XmlNode& node) {

	push();
	const map<string, string>& attributes = node.attributes();
	if (attributes.find("key") != attributes.end() ) {
		string result = owner_.get("spot", attributes.find("key")->second);
		check(owner_.get("spot", attributes.find("key")->second));
	}
	node.visit(*this);
	pop();
}
*/

void TagConverter::json(const XmlNode& node) {

	push();
	const map<string, string>& attributes = node.attributes();
	if (attributes.find("key") != attributes.end() ) {
		string result = owner_.get("json", attributes.find("key")->second);
		check(owner_.get("json", attributes.find("key")->second));
	}
	node.visit(*this);
	pop();

	font_ = top().font();
	elevation_ = top().elevation();
}
void TagConverter::spot(const XmlNode& node) {

	push();
	const map<string, string>& attributes = node.attributes();
	if (attributes.find("key") != attributes.end() ) {
		string result = owner_.get("spot", attributes.find("key")->second);
		check(owner_.get("spot", attributes.find("key")->second));
	}
	node.visit(*this);
	pop();

	font_ = top().font();
	elevation_ = top().elevation();
}
void TagConverter::netcdf(const XmlNode& node) {

	push();
	const map<string, string>& attributes = node.attributes();
	string key =  ( attributes.find("variable") != attributes.end() ) ? attributes.find("variable")->second : "";
	if (attributes.find("attribute") != attributes.end() ) {
		string result = owner_.get("netcdf"+key, attributes.find("attribute")->second);
		if ( result.empty() ) {
			if ( automatic_ == 1 )
				automatic_ = 0;
		}
		else {
			if ( automatic_ == 0 )
				automatic_ = 1;
			automatic_++;
			check(owner_.get("netcdf"+key, attributes.find("attribute")->second));
		}
	}
	if (attributes.find("definition") != attributes.end() ) {
		check(owner_.get("netcdf"+key, attributes.find("definition")->second));
	}
	node.visit(*this);
	pop();

	font_ = top().font();
	elevation_ = top().elevation();
}

void TagConverter::pass(const XmlNode& node) { 
	
	
	node.visit(*this);
	check(node.data()); 

	
}
void TagConverter::magics(const XmlNode& node) { 
	//MagLog::dev()<< "checking magics title!" << endl;
	push();		
	const map<string, string>& attributes = node.attributes();
	string key =  ( attributes.find("id") != attributes.end() ) ? attributes.find("id")->second : ""; 
    
    check(owner_.get("grib"+key, "magics")); 
 
	node.visit(*this);
	pop();	
	
	font_ = top().font();
	elevation_ = top().elevation();
}

void TagConverter::base_date(const XmlNode& node) { 
	push();		
    
    check(owner_.get("grib", "base_date")); 
 
	node.visit(*this);
	pop();	
	
	font_ = top().font();
	elevation_ = top().elevation();
}

void TagConverter::data(const XmlNode& node) { 
	
	
	push();
	const map<string, string>& attributes = node.attributes();
	if ( attributes.find("data") != attributes.end() ) {
		    string text = attributes.find("data")->second;
		    if (!text.empty() ) {		
		    	top().text(text);		
		    	label_ += text;
		    	text_->addNiceText(top());
		    }
	}
	node.visit(*this);
	pop();	
	
	font_ = top().font();
	elevation_ = top().elevation();
	
}
void TagConverter::font(const XmlNode& node) {
	const map<string, string>& attributes = node.attributes();
	
	if ( attributes.find("colour") != attributes.end() ) {
		MagLog::debug() << "set colour" << attributes.find("colour")->second << endl;
		font_.colour(Colour(attributes.find("colour")->second));
	}

	if ( attributes.find("color") != attributes.end() ) {
		MagLog::debug() << "set colour" << attributes.find("color")->second << endl;
		font_.colour(Colour(attributes.find("color")->second));
	}
		
	if ( attributes.find("size") != attributes.end() ) {		
		font_.size(tonumber(attributes.find("size")->second));	
		MagLog::debug() << "set size" << attributes.find("size")->second << endl;
	}
	
	if ( attributes.find("style") != attributes.end() ) {		
		font_.style(attributes.find("style")->second);	
		MagLog::debug() << "set style" << attributes.find("style")->second << endl;
	}
	

	node.visit(*this);
	
	
	

}

void TagConverter::underline(const XmlNode& node) {
	MagLog::debug() << "uderline" << endl;
	font_.style("underlined");

	node.visit(*this);

}
void TagConverter::push()
{
	NiceText nice = top();
	nice.font(font_);
	nice.elevation(elevation_);
	stack<NiceText>::push(nice);
}
void TagConverter::bold(const XmlNode& node) {
	MagLog::debug() << "bold" << endl;
	font_.style("bold");		
	node.visit(*this);

}
void TagConverter::superscript(const XmlNode& node) {
	MagLog::debug() << "superscript" << endl;
	elevation_ = SUPERSCRIPT;
	node.visit(*this);
}
void TagConverter::subscript(const XmlNode& node) {
	MagLog::debug() << "subcript" << endl;
	elevation_ = SUBSCRIPT;
	node.visit(*this);
}
void TagConverter::check(const string& text) {
	

	if (!text.empty() ) {		
		TagConverter converter(owner_);
		converter.font(font_);
		converter.decode(text, text_);
		label_ = text;
	}
	

	
}

void TagConverter::entities(ostream& out) 
{
	string path = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "entities.dtd";

	try {
		ifstream in(path.c_str());		
		char c;
		while(in.get(c))
		{
			out << c;
		}
		in.close();
	}
	catch (...) { 
		MagLog::warning() << "Cannot find entities declaration : " << path << " [Ignore them]" << endl;
	}
}

bool TagConverter::staticTag(const string& line)
{
	static vector<string> keys;
	if ( keys.empty() ) {
		keys.push_back("grib_info");
		keys.push_back("netcdf_info");
		keys.push_back("json_info");
		keys.push_back("spot_info");
		keys.push_back("magics_title");
	}
	for ( vector<string> ::iterator key = keys.begin(); key !=keys. end(); ++key)  {
		string meta = "<" + *key;
		if ( line.find(meta) != string::npos ) 
			  return false;
	}
	// No MetaTag found!
	return true;
}

void TagConverter::decode(const string& line, Text* text)
{
	text_ = text;
	elevation_ = NORMAL;

	NiceText nice;
	nice.font(font_);
	nice.elevation(elevation_);
	stack<NiceText>::push(nice);
	XmlReader parser(true);
	XmlTree tree;	

	ostringstream xml;
	xml << "<?xml version='1.0' ?> \n";
	entities(xml);
	xml << "<xml> \n";
	xml << line << "\n";
	xml << "\n</xml>";
	automatic_ = 1;



	if ( parser.decode(xml.str(), &tree) == 0 )
		tree.visit(*this);
	else {

		//label_ += line;
		text_->addText(line, font_);
	}
	if (automatic_ == 0 ) {
		text->clear();
	}

}


