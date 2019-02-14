/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LocalTable.h
    \brief Implementation of the Template class LocalTable.

    Magics Team - ECMWF 2004

    Started: Mon 21-Jun-2004

    Changes:

*/
#include "LocalTable.h"
#include <fstream>
#include "MagException.h"
#include "expat.h"
//#include "MagLog.h"

using namespace magics;
template <>
map<string, DefinitionTable<ParamDef>*>* DefinitionTable<ParamDef>::tables_ = 0;

const DefinitionTable<ParamDef>& LocalTable::localTable(long table, long) {
    ostringstream sfile;
    sfile << "table_" << table << ".xml";
    return definitionTable(sfile.str(), "param");
}

const ParamDef& LocalTable::localInfo(long code, long table, long centre) {
    const DefinitionTable<ParamDef>& local = localTable(table, centre);
    return local.definition(code);
}

LocalTable::~LocalTable() {}

/*!
 Class information are given to the output-stream.
*/
void LocalTable::print(ostream& out) const {
    out << "LocalTable[";
    out << "]";
}

ParamDef::ParamDef(const map<string, string>& def) {
    map<string, string>::const_iterator info;

    info = def.find("code");
    if (info == def.end())
        code_ = -1;
    else
        code_ = atoi(info->second.c_str());

    info = def.find("long_title");
    if (info == def.end())
        longTitle_ = "Unknown parameter";
    else
        longTitle_ = info->second;

    info = def.find("short_title");
    if (info == def.end())
        shortTitle_ = -1;
    else
        shortTitle_ = info->second;

    info = def.find("original_unit");
    if (info == def.end())
        originalUnit_ = "Unknown unit";
    else
        originalUnit_ = info->second;

    info = def.find("derived_unit");
    if (info == def.end())
        derivedUnit_ = "Unknown unit";
    else
        derivedUnit_ = info->second;

    info = def.find("scaling");
    if (info == def.end())
        scaling_ = 1;
    else
        scaling_ = atof(info->second.c_str());

    info = def.find("offset");
    if (info == def.end())
        offset_ = 1;
    else
        offset_ = atof(info->second.c_str());
}

void ParamDef::print(ostream& out) const {
    out << "ParamDef[";
    out << "code=" << code_;
    out << ", long title=" << longTitle_;
    out << ", short title=" << shortTitle_;
    out << ", original units=" << originalUnit_;
    out << ", derived units=" << derivedUnit_;
    out << ", scaling factor=" << scaling_;
    out << ", offset=" << offset_;
    out << "]";
}
