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
void setAttribute(const vector<string>& roots, const string& name, auto_ptr<Colour>&, const map<string, string>&);

template <class T>
void setMember(const string& value, auto_ptr<T>& object, const XmlNode& from)
{
	try{
		T* new_object = MagTranslator<string, T>()(value);
		
        if (new_object == 0 )  {
            object->set(from);
            MagLog::dev()<< "OK" << endl;
            return;
        }
        object = auto_ptr<T>(new_object);
        
        
	}
	catch (...) {
       
	}
	object->set(from);;
}

template <class T>
bool acceptNode(const string& node, auto_ptr<T>& object)
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
void setMember(const vector<string>& roots, const string& name, auto_ptr<T>& object, const map<string, string>& params)
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
			object = auto_ptr<T>(new_object);
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
