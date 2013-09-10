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
	return make_pair(var, current);
}





string replace(const string& in, const map<string, string>& variables)
{
	pair<string, string> var = cut(in);

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
