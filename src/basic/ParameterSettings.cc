/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ParameterSettings.cc
    \brief Implementation of parameter settings.
    \author Meteorological Visualisation Section, ECMWF

    Started: 2004

*/
#include <ParameterSettings.h>
#include <Path.h>

void buildkeys(const vector<string>& roots, const string& name, vector<string>& keys)
{
	keys.push_back(name);

	for (vector<string>::const_iterator root = roots.begin(); root != roots.end(); ++root)
	{
		string x = name;
		string::size_type index = x.find(*root);

		if ( index != string::npos)
		{
			x.replace(index, (*root).length()+1, "");
			keys.push_back(x);		
		}
	}
}

void setAttribute(const vector<string>& roots, const string& name, string& param, const map<string, string>& params)
{
	vector<string> keys;	
	buildkeys(roots, name, keys);

	map<string, string>::const_iterator val;
	for (vector<string>::const_iterator key = keys.begin(); key != keys.end(); ++key)
	{
		val = params.find(*key);

		if ( val == params.end() ) continue;
		MagLog::debug() << "Parameter [" << name << "] set to " << val->second << endl;
		param = val->second;
	}
}

void setAttribute(const vector<string>& roots, const string& name, auto_ptr<Colour>& param, const map<string, string>& params)
{
	vector<string> keys;	
	buildkeys(roots, name, keys);

	map<string, string>::const_iterator val;
	for (vector<string>::const_iterator key = keys.begin(); key != keys.end(); ++key)
	{
		val = params.find(*key);
		if ( val == params.end() ) continue;
		MagLog::debug() << "Parameter [" << name << "] set to " << val->second << endl;
		param = auto_ptr<Colour>(new Colour(val->second));
	}
}

void niceprint(ostream& out, const string& val)
{
	out << "\"" << val << "\"";
}



template <>
void niceprint(ostream& out, const AxisAutomaticSetting& val )
//{m_off, m_both, m_min_only, m_max_only};)
{
	string cval;
	switch (val) {
		case m_off: cval="off";
				break;
		case m_both: cval="on";
				break;
		case m_min_only: cval="min_only";
				break;
		case m_max_only: cval="max_only";
						break;

	}
	niceprint(out, cval);
}
template <>
void niceprint(ostream& out, const Colour& val )
//{m_off, m_both, m_min_only, m_max_only};)
{

	niceprint(out, val.name());
}

void toxml(string& out, const map<string, string>& def)
{
	ostringstream os;
	string sep = "";
	for (map<string, string>::const_iterator entry = def.begin(); entry != def.end(); ++entry ) {
		os << sep << "\"" << entry->first << "\" : \"" << entry->second << "\"";
		sep = ",\n";
	}

	out = os.str();
}
