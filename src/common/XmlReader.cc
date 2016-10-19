/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file XmlReader.cc
    \brief Implementation of the Reader for Xml.
    
    Magics Team - ECMWF 2005
    
    Started: 2005
    
    Changes:
    
*/ 

#include "XmlReader.h"
#include "expat.h"
#include "MagLog.h"
#include "MagException.h"
using namespace magics;

XmlReader::XmlReader(bool tag) : dataAsTag_(tag) 
{
}


XmlReader::~XmlReader() 
{
}

static void XMLCALL startElementHandler(void *data, const char *name, const char **atts)
{
	XmlReader* reader  = (XmlReader*) data; 
	map<string, string> def;
	while (*atts) {         
		def.insert(std::make_pair(*atts,*(atts+1)));
		atts+=2;
	}	
	reader->newElement(name, def);      
}


static void XMLCALL endElementHandler(void* data, const char* tag)
{
	XmlReader* reader  = (XmlReader*) data; 
	reader->endElement(tag);
}

static bool empty(const string& value)
{
	for (string::const_iterator c = value.begin(); c != value.end(); ++c) {
		if ( !isspace(*c) ) return false;
	}
	return true;
}

static void XMLCALL dataHandler (void *data, const char *value, int len)
{
	string stringval = string(value, len);
	if (  empty(stringval) ) return;
	XmlReader* reader  = (XmlReader*) data;
	
	if ( reader->dataAsTag() ) {
		map<string, string> def;
    	def["data"] = stringval;
		reader->newElement("data", def);     
		reader->endElement("data");
	}
	else 
		reader->addData(stringval);
}


static int externalEntityRefHandler( XML_Parser ,//	 parser,
		const XML_Char *  	context,
		const XML_Char *  	base,
		const XML_Char *  	systemID,
		const XML_Char *  	publicID
	)
{
	//MagLog::dev()<< "context--->" << context << endl;
	//MagLog::dev()<< "base--->" << base << endl;
	//MagLog::dev()<< "systemID--->" << systemID << endl;
	//MagLog::dev()<< "publicID--->" << publicID << endl;
	return 0;
}



void XmlReader::newElement(const string& name, const map<string, string>& def)
{

	// Here we create a XmlNode you put in the tree...
	XmlNode* parent = tree_->getParent(name);
	if ( parent ) {
		//MagLog::dev()<< "push-->empty " << name << endl;
		push(parent);
	}
	if ( empty() )
		push(tree_->root());
	XmlNode* node = tree_->newNode(name, def);
	top()->push_back(node);
	push(node); 
}

void XmlReader::endElement(const string& tag)
{
	tree_->endElement(tag);
	pop();
	//MagLog::dev()<< "pop-->" << tag << endl;
	if ( tag == "definition" ) {
		pop();
		//MagLog::dev()<< "pop-->empty" << tag << endl;
	}
	if ( tag == "magics" ) {
		pop();
		//MagLog::dev()<< "pop-->empty" << tag << endl;
	}
}

void XmlReader::addData(const string& data)
{
	top()->setData(data);
}



void XmlReader::interpret(const string& xml, XmlTree* tree)
{
	tree_ = tree;
	char buf[BUFSIZ];
	XML_Parser parser = XML_ParserCreate(NULL);
	int done;
	XML_SetUserData(parser, this);
	XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
	XML_SetElementHandler(parser, startElementHandler, endElementHandler);
	XML_SetCharacterDataHandler(parser, dataHandler);

	FILE* in  = fopen(xml.c_str(), "r");

	if (!in) {
		MagLog::dev()<< "XmlDecoder: can not open file " << xml << endl;
		MagLog::error() << "XmlDecoder: can not open file " << xml << endl;
		return;
	}

	do
	{
		size_t len = fread(buf, 1, sizeof(buf), in);
		done = len < sizeof(buf);
		if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR)
		{
			ostringstream s;
			s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser))  << " at line  " <<  XML_GetCurrentLineNumber(parser)  << ends;
			cerr <<  s.str() << "\n";
		}
	} while (!done);
	XML_ParserFree(parser);
	fclose(in);
}

int XmlReader::decode(const string& xml, XmlTree* tree)
{
	tree_ = tree;
//	char buf[BUFSIZ];
	XML_Parser parser = XML_ParserCreate(NULL);
//	int done;
	XML_SetUserData(parser, this);
	XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
	XML_SetElementHandler(parser, startElementHandler, endElementHandler);
	XML_SetCharacterDataHandler(parser, dataHandler);
		
	if (XML_Parse(parser, xml.c_str(), xml.size(), true) == XML_STATUS_ERROR)
	{
		ostringstream s;
		s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser))  << " at line  " <<  XML_GetCurrentLineNumber(parser)  << ends;
		return -1;
	}
	XML_ParserFree(parser);
	return 0;
}


/*!
 Class information are given to the output-stream.
*/		
void XmlReader::print(ostream& out)  const
{
	out << "XmlReader[";
	out << "]";
}
