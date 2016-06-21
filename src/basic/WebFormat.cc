/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "WebFormat.h"
#include "MagJSon.h"
using namespace magics;
#include "Timer.h"

pair<string, string> cut(const string& in)
{
	string::const_iterator c = in.begin();
	string var;
	string current;
	
	while ( c != in.end() )
	{		
		if ( *c == '=' ) {			
			var = current;
			current = "";
		}
		else {
			current.push_back(*c);
		}
		c++;
	}
	if ( var.empty() ) {
		var = current;
		current = "";
	}
	return std::make_pair(var, current);
}





string replace(const string& in, const map<string, string>& variables)
{
	std::pair<string, string> var = cut(in);

	map<string, string>::const_iterator variable = variables.find(var.first);

	return ( variable != variables.end() ) ? variable->second : var.second;
}

int findandreplace(string& in, int from, const map<string, string>& variables)
{
	for (map<string, string>::const_iterator var = variables.begin(); var != variables.end(); ++var)
		if ( in.substr(from+1, var->first.size()) == var->first) 
		{
			in.replace(from, var->first.size()+1, var->second);
			return from+var->second.size();
		}
	return from;
}
static void substitute(string& xml, const map<string, string>& variables) 
{
	for (unsigned int i = 0; i < xml.size(); i++)
	{
		if ( xml[i] == '$' ) {
			if ( xml[i+1] == '{' ) {
				string var;
				unsigned int last  = i+2;
			    while ( xml[last] != '}' ) {
			    	var.push_back(xml[last]);
			    	last++;
			    }
			    last++;
			    string val = replace(var, variables);
			    xml.replace(i, last-i, val);
			    i = i + val.length();
			}
			else {
				i = findandreplace(xml, i, variables);
			}
		}
	}
}



void WebFormat::prepare(const string& magml, const map<string, string>& params, TempFile& file)
{
	ifstream in(magml.c_str());
	if ( !in )
	{
		MagLog::error() << " Can not open file " << magml << endl;
		throw (MagicsException("no file"));
	} 

	ofstream& out = file();

	string s = "";
	char c;
	while(in.get(c)) s+=c;

	substitute(s, params);
	out << s; 

	in.close();
	out.flush();
	out.close();
};

void MagML::execute(const string& magml, const map<string, string>& params)
{
	XmlMagics magics;
	TempFile file;
	prepare(magml, params, file);	
	magics.execute(file.name());
}

WebInterpretor WebInterpretor::web_;

WebInterpretor::WebInterpretor()
{
}

WebInterpretor::~WebInterpretor()
{
}

void WebInterpretor::magml(const string& file)
{
	MagML magml;
	magml.execute(file, web_);
	web_.clear(); 
}

void WebInterpretor::json(const string& file)
{

	MagJSon json;
	json.execute(file, web_);
	web_.clear();
}
