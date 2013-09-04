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

/*! \file LocalTable.cc
    \brief Implementation of the Template class LocalTable.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/
#include "LocalTable.h"
#include "MagException.h"
#include "expat.h"
#include <stdio.h>
#include <string>
using std::basic_string;

using namespace magics;


static void XMLCALL
startElement(void *userData, const char *name, const char **atts)
{
	BaseTable* table  = (BaseTable*) userData; 
	if (string(name) == table->info() )
	{	
        map<string, string> def;
        while (*atts) {
            def[*(atts)] = *(atts+1);
            atts+=2;
        }
        table->add(def);
	}
}

static void XMLCALL
endElement(void *, const char *)
{
}

//template <class D> 
//map<string, DefinitionTable<D>* >* DefinitionTable<D>::tables_ = 0;

template <class D> 
D DefinitionTable<D>::unknown_;
 
template <class D> 
DefinitionTable<D>::DefinitionTable(const string& file, const string& keyword) : BaseTable(keyword) 
{
	string filename = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + file;
	char buf[BUFSIZ];
	XML_Parser parser = XML_ParserCreate(NULL);
	int done;
	XML_SetUserData(parser, this);
	XML_SetElementHandler(parser, startElement, endElement);

	FILE* in  = fopen(filename.c_str(), "r");

	if (!in) return;

	do
	{
		size_t len = fread(buf, 1, sizeof(buf), in);
		done = len < sizeof(buf);
		if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR)
		{
			ostringstream s;
			s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser))  << " at line  " <<  XML_GetCurrentLineNumber(parser)  << ends;
			cerr <<  s.str() << "\n";
			//throw MagicsException(s.str());
		}
	} while (!done);
	XML_ParserFree(parser);
	fclose(in);
}

template <class D> 
const DefinitionTable<D>& DefinitionTable<D>::definitionTable(const string& file, const string& keyword)
{
    if (!tables_) tables_ = new map<string, DefinitionTable<D>* >();
    typename map<string, DefinitionTable<D>*>::const_iterator local =  tables_->find(file);
    if ( local == tables_->end() )
    {
       DefinitionTable<D>* newlocal = new DefinitionTable<D>(file, keyword);
       (*tables_)[file] = newlocal;
       return *newlocal;
    }
    return *(local->second);
}

template <class D>
const D& DefinitionTable<D>::definitionInfo(const string& file, const string& keyword, int code)
{
    const DefinitionTable<D>& local = definitionTable(file, keyword); 
    return local.definition(code);    
}

template <class D>
DefinitionTable<D>::~DefinitionTable() 
{}

/*!
 Class information are given to the output-stream.
*/	
template <class D>    
void DefinitionTable<D>::print(ostream& out)  const
{
	out << "DefinitionTable[";
	out << "]";
}
