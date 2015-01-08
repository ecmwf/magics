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

/*! \file TitleTemplate.cc
    \brief Implementation of the Template class TitleTemplate.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/
#include "TitleTemplate.h"
#include "Factory.h"
#include "MagException.h"
#include "expat.h"
#include "TitleMetaField.h"
#include "TitleStringField.h"
#include "GribDecoder.h"
using namespace magics;

static bool ignore_space_;
static void XMLCALL
startElement(void *userData, const char *name, const char **atts)
{
	TitleTemplate* object  = (TitleTemplate*) userData; 

	if (string(name) == "title" )
	{
		TitleTemplate* title = new TitleTemplate();
		while (*atts) {
            title->criteria()[*(atts)] = *(atts+1);           
            atts += 2;
		}
		object->top()->push_back(title);
		object->push(title);
		ignore_space_ = true;
		return;
	}
	if (string(name) == "text"  )
		ignore_space_ = false;
	else
	{
		TitleMetaField* meta = new TitleMetaField(name);
		while (*atts)
        {
            (*meta)[*(atts)] = *(atts+1);           
            atts += 2;
		}
		object->top()->add(meta);
	}

}

static void XMLCALL
endElement(void *userData, const char *name)
{

	if (string(name) == "title" )
	{
		TitleTemplate* object  = (TitleTemplate*) userData;
		object->pop();
		ignore_space_ = true;

	}
	if (string(name) == "text"  )
			ignore_space_ = true;
}



static void XMLCALL character (void *userData,
                            const char *s,
                            int len)
{


	string data(s, len);

	TitleTemplate* object  = (TitleTemplate*) userData;
	if (  data == "\n" ) return;
	if( ignore_space_ && data.find_first_not_of(" \n\t") == std::string::npos)
		return;

	object->top()->add(new TitleStringField(data));
}

static void XMLCALL startData(void *)
{
	MagLog::dev() << "start data" << "\n";
	
}

static void XMLCALL endData(void *)
{
}

TitleTemplate* TitleTemplate::singleton_ = 0;

TitleTemplate::TitleTemplate() 
{
	if ( !singleton_ ) decode();
}

void TitleTemplate::decode()
{
	singleton_ = this;
	string filename = getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + file_;
	char buf[BUFSIZ];
	ignore_space_ = true;
	push(this);
	XML_Parser parser = XML_ParserCreate(NULL);
	int done;
	XML_SetUserData(parser, this);
	XML_SetElementHandler(parser, startElement, endElement);
	XML_SetCdataSectionHandler(parser,startData, endData);
	XML_SetCharacterDataHandler(parser, character);

	FILE* in  = fopen(filename.c_str(), "r");
     
	if (!in) throw NoSuchFileException(filename);
	
	do
	{
		size_t len = fread(buf, 1, sizeof(buf), in);
		done = len < sizeof(buf);
		if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR)
		{
			ostringstream s;
			s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser))  << " at line  " <<  XML_GetCurrentLineNumber(parser)  << ends;
			MagLog::error() << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser))  << " at line  " <<  XML_GetCurrentLineNumber(parser)  << "\n";
            throw MagicsException(s.str());
		}
	} while (!done);
	XML_ParserFree(parser);
	fclose(in);
}

TitleTemplate::~TitleTemplate() 
{}

/*!
 Class information are given to the output-stream.
*/		
void TitleTemplate::print(ostream& out)  const
{
	out << "TitleTemplate[";
	for (map<string, string>::const_iterator criter = criteria_.begin(); criter != criteria_.end(); ++criter)
	{
		out << criter->first << " = " << criter->second << "," << "\n";
	}
	for (vector<TitleField*>::const_iterator field = template_.begin(); field != template_.end(); ++field) 
		out << *(*field);   
	for (const_iterator child = begin(); child != end(); ++child)
		out << *(*child);   
	out << "]";
}

bool TitleTemplate::verify(const GribDecoder& data) const
{
	for (map<string, string>::const_iterator criter = criteria_.begin(); criter != criteria_.end(); ++criter)
	{
//#ifdef MAGICS_EXCEPTION
		try{
			MagLog::debug() << "Try  to create the MatchCriteria for " << criter->first << "\n";
			auto_ptr<MatchCriteria >  object(SimpleObjectMaker<MatchCriteria >::create(criter->first));
			MagLog::debug() << "Found the MatchCriteria for " << criter->first << "\n";
			if (!(*object).verify(data, criter->first, criter->second)) return false;
		}
		catch (NoFactoryException& e)
		{ // The data do not know how to verify the criter ....
			MagLog::warning() << "Can Not Create the MatchCriteria for " << criter->first << "\n";
			return false;
		}
//#else

//#endif
	}
	return true;
}

