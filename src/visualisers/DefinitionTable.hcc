/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
