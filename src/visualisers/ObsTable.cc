/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsTable.cc
    \brief Implementation of the Template class LocalTable.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/
#include "ObsTable.h"
#include "MagException.h"
#include "expat.h"
#include <string>
#include "Layout.h"
#include "Transformation.h"
#include "Symbol.h"

#include "ProgressObject.h"

using std::basic_string;

using namespace magics;


static void XMLCALL
startElement(void *userData, const char *name, const char **atts)
{
	ObsTable* table  = (ObsTable*) userData; 
        map<string, string> def;
        while (*atts) {
            def[*(atts)] = *(atts+1);
            atts+=2;
        }
        table->add(name, def);
	
}

static void XMLCALL
endElement(void *, const char *)
{
}

ObsTable* ObsTable::table_ = 0;


ObsTable::ObsTable() 
{
 
    string filename = path_.empty() ? getEnvVariable("MAGPLUS_HOME") + MAGPLUS_PATH_TO_SHARE_ + "obs.xml" : path_;
	char buf[BUFSIZ];
	XML_Parser parser = XML_ParserCreate(NULL);
	int done;
	XML_SetUserData(parser, this);
	XML_SetElementHandler(parser, startElement, endElement);

	MagLog::dev() << "Load observation templates --->" << filename << endl;
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
			cerr <<  s.str() << "\n";
			//throw MagicsException(s.str());
		}
	} while (!done);
	XML_ParserFree(parser);
	fclose(in);
}


ObsTable::~ObsTable() 
{}

/*!
 Class information are given to the output-stream.
*/	

    
void ObsTable::print(ostream& out)  const
{
	out << "ObsTable[";	
	for ( const_iterator item = begin(); item != end(); ++item) 
			    out << "\t" << item->first << "---->" << *(item->second) << "\n";
			out << "\n";
	out << "]";
}

void ObsTable::add(const string& tag, const map<string, string>& def) 
{
 	if ( tag == "observations" ) return;
 	if ( tag == "obs_template" ) {
 		current_ = new ObsTemplate(def);
 		insert(make_pair(def.find("type")->second, current_));
 	} 
 	else {
 		try {
 			ObsItem* obs = SimpleObjectMaker<ObsItem>::create(tag);
 			obs->set(def);
 			current_->push_back(obs);
 		}
 		catch (NoFactoryException&) {
 			MagLog::dev() << "can not find ObsItem for : " << tag << "\n";
 		}
 	}
}
   
const ObsTemplate& ObsTable::get(const string& type)
{
	const_iterator entry = find(type);
	if ( entry != end() ) { return *(entry->second); }
	else entry =  find("position");
	return *(entry->second); 
	throw MagicsException();
}   


void ObsTemplate::operator()(CustomisedPoint& obs, BasicGraphicsObjectContainer& out) const
{
		if ( empty() ) 
			return; // Nothing to display.
			
	

		const Transformation& transformation = out.transformation();
		
		PaperPoint pp = transformation(UserPoint(obs.longitude(), obs.latitude()));
	
		
		ComplexSymbol* symbol = new ComplexSymbol(rows_, columns_);
		
		symbol->push_back(pp);
		symbol->setDistanceApart(apart_);

		symbol->setHeight(height_*0.5);
		
		out.push_back(symbol);

		for ( const_iterator item = begin(); item != end(); ++item) 
			    (*(*item))(obs, *symbol);
}
