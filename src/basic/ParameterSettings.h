/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ParameterSettings.h
    \brief Definition of parameter settings.
    \author Meteorological Visualisation Section, ECMWF

    Started: 

*/
#ifndef ParameterSet_H
#define ParameterSet_H

#include <magics.h>
#include <MagTranslator.h>
#include <XmlNode.h>
#include <Colour.h>

void buildkeys(const vector<string>& roots, const string&, vector<string>& keys);
void setAttribute(const vector<string>& roots, const string& name, unique_ptr<Colour>&, const map<string, string>&);

template <class T>
void setMember(const string& value, unique_ptr<T>& object, const XmlNode& from)
{
	try{
		T* new_object = MagTranslator<string, T>()(value);
		
        if (new_object == 0 )  {
            object->set(from);
            MagLog::dev()<< "OK" << endl;
            return;
        }
        object = unique_ptr<T>(new_object);
        
        
	}
	catch (...) {
       
	}
	object->set(from);;
}

template <class T>
bool acceptNode(const string& node, unique_ptr<T>& object)
{
	try {
		T* new_object = MagTranslator<string, T>()(node);
        if (new_object == 0) 
            return object->accept(node);;		
		delete new_object;
		return true;
	}
	catch (...) {
		return object->accept(node);
	}	
}

template <class T>
void setMember(const vector<string>& roots, const string& name, unique_ptr<T>& object, const map<string, string>& params)
{	
	vector<string> keys;
	buildkeys(roots, name, keys);
	map<string, string>::const_iterator val;

	for (vector<string>::const_iterator key = keys.begin(); key != keys.end(); ++key)
	{
		val = params.find(*key);
		if ( val == params.end() ) continue;
		try {
			T* new_object = MagTranslator<string, T>()(val->second);
            if (new_object == 0)  {
                object->set(params);
                return;
            }
			object = unique_ptr<T>(new_object);
			MagLog::debug() << "Parameter [" << name << "] set to " << val->second << endl;
		}
		catch (...) {
        }
	}
	object->set(params);
}

template <class T>
void setAttribute(const vector<string>& roots, const string& name, T& param, const map<string, string>& params)
{
	vector<string> keys;	
	buildkeys(roots, name, keys);

	map<string, string>::const_iterator val;
	for (vector<string>::const_iterator key = keys.begin(); key != keys.end(); ++key) {
		val = params.find(*key);
		
		if ( val == params.end() ) continue;
		MagLog::debug() << "Parameter [" << name << "] set to " << val->second << endl;
		param = MagTranslator<string, T>()(val->second);
	}	 
}

template <class T>
void niceprint(ostream& out, const T& val) {
	out << val;
}

void niceprint(ostream& out, const string&); 

void toxml(string&, const map<string, string>&);
#endif
