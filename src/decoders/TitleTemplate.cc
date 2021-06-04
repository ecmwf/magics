/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TitleTemplate.cc
    \brief Implementation of the Template class TitleTemplate.

    Magics Team - ECMWF 2004

    Started: Mon 21-Jun-2004

    Changes:

*/
#include "TitleTemplate.h"
#include "Factory.h"
#include "GribDecoder.h"
#include "MagException.h"
#include "TitleMetaField.h"
#include "TitleStringField.h"
#include "MagicsGlobal.h"

#include "expat.h"
using namespace magics;

static bool ignore_space_;
static void XMLCALL startElement(void* userData, const char* name, const char** atts) {
    TitleTemplate* object = (TitleTemplate*)userData;

    if (string(name) == "title") {
        TitleTemplate* title = new TitleTemplate();
        while (*atts) {
            title->criteria()[*(atts)] = *(atts + 1);
            atts += 2;
        }
        object->top()->push_back(title);
        object->push(title);
        ignore_space_ = true;
        return;
    }
    if (string(name) == "text")
        ignore_space_ = false;
    else {
        TitleMetaField* meta = new TitleMetaField(name);
        while (*atts) {
            (*meta)[*(atts)] = *(atts + 1);
            atts += 2;
        }
        object->top()->add(meta);
    }
}

static void XMLCALL endElement(void* userData, const char* name) {
    if (string(name) == "title") {
        TitleTemplate* object = (TitleTemplate*)userData;
        object->pop();
        ignore_space_ = true;
    }
    if (string(name) == "text")
        ignore_space_ = true;
}


static void XMLCALL character(void* userData, const char* s, int len) {
    string data(s, len);

    TitleTemplate* object = (TitleTemplate*)userData;
    if (data == "\n")
        return;
    if (ignore_space_ && data.find_first_not_of(" \n\t") == std::string::npos)
        return;

    object->top()->add(new TitleStringField(data));
}

static void XMLCALL startData(void*) {
    MagLog::dev() << "start data"
                  << "\n";
}

static void XMLCALL endData(void*) {}

TitleTemplate* TitleTemplate::singleton_ = 0;

TitleTemplate::TitleTemplate() {
    if (!singleton_)
        decode();
}

void TitleTemplate::decode() {
    singleton_      = this;
    string filename = buildSharePath(file_);
    char buf[BUFSIZ];
    ignore_space_ = true;
    push(this);
    XML_Parser parser = XML_ParserCreate(NULL);
    int done;
    XML_SetUserData(parser, this);
    XML_SetElementHandler(parser, startElement, endElement);
    XML_SetCdataSectionHandler(parser, startData, endData);
    XML_SetCharacterDataHandler(parser, character);

    FILE* in = fopen(filename.c_str(), "r");

    if (!in) {
        throw CannotOpenFile(filename);
    }

    do {
        size_t len = fread(buf, 1, sizeof(buf), in);
        done       = len < sizeof(buf);
        if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR) {
            ostringstream s;
            s << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser)) << " at line  "
              << XML_GetCurrentLineNumber(parser) << ends;
            MagLog::error() << "XmlMagException : " << XML_ErrorString(XML_GetErrorCode(parser)) << " at line  "
                            << XML_GetCurrentLineNumber(parser) << "\n";
            throw MagicsException(s.str());
        }
    } while (!done);
    XML_ParserFree(parser);
    fclose(in);
}

TitleTemplate::~TitleTemplate() {}

/*!
 Class information are given to the output-stream.
*/
void TitleTemplate::print(ostream& out) const {
    out << "TitleTemplate[";
    for (map<string, string>::const_iterator criter = criteria_.begin(); criter != criteria_.end(); ++criter) {
        out << criter->first << " = " << criter->second << ","
            << "\n";
    }
    for (auto& field : template_)
        out << *field;
    for (const_iterator child = begin(); child != end(); ++child)
        out << *(*child);
    out << "]";
}

bool TitleTemplate::verify(const GribDecoder& data) const {
    for (map<string, string>::const_iterator criter = criteria_.begin(); criter != criteria_.end(); ++criter) {
        try {
            MagLog::debug() << "Try  to create the MatchCriteria for " << criter->first << "\n";
            unique_ptr<MatchCriteria> object(SimpleObjectMaker<MatchCriteria>::create(criter->first));
            MagLog::debug() << "Found the MatchCriteria for " << criter->first << "\n";
            if (!(*object).verify(data, criter->first, criter->second))
                return false;
        }
        catch (NoFactoryException& e) {  // The data do not know how to verify the criter ....
            if (MagicsGlobal::strict()) {
                throw;
            }
            MagLog::warning() << "Can Not Create the MatchCriteria for " << criter->first << "\n";
            return false;
        }
    }
    return true;
}
