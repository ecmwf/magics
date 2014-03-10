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

/*! \file LocalTable.h
    \brief Implementation of the Template class LocalTable.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/
#include <fstream>
#include "LocalTable.h"
#include "MagException.h"
#include "expat.h"
//#include "MagLog.h"

using namespace magics;
template <> 
map<string, DefinitionTable<ParamDef>* >* DefinitionTable<ParamDef>::tables_ = 0;

const DefinitionTable<ParamDef>& LocalTable::localTable(long table, long)
{
    ostringstream sfile;
	sfile << "table_" << table << ".xml";
    return definitionTable(sfile.str(), "param");
}

const ParamDef& LocalTable::localInfo(long code, long table, long centre)
{
    const DefinitionTable<ParamDef>& local = localTable(table, centre); 
    return local.definition(code);    
}

LocalTable::~LocalTable() 
{}

/*!
 Class information are given to the output-stream.
*/		
void LocalTable::print(ostream& out)  const
{
	out << "LocalTable[";
	out << "]";
}

ParamDef::ParamDef(const map<string, string>& def)
{
    map<string, string>::const_iterator info;
    
    info = def.find("code");
    if ( info == def.end() ) code_ = -1;
    else code_ =  atoi(info->second.c_str());
    
    info = def.find("long_title");
    if ( info == def.end() ) longTitle_ = "Unknown parameter";
    else longTitle_ =  info->second;
    
    info = def.find("short_title");
    if ( info == def.end() ) shortTitle_ = -1;
    else shortTitle_ =  info->second;
    
    info = def.find("original_unit");
    if ( info == def.end() ) originalUnit_ = "Unknown unit";
    else originalUnit_ =  info->second;
    
    info = def.find("derived_unit");
    if ( info == def.end() ) derivedUnit_ = "Unknown unit";
    else derivedUnit_ =  info->second;
    
    info = def.find("scaling");
    if ( info == def.end() ) scaling_ = 1;
    else scaling_ =   atof(info->second.c_str());
    
    info = def.find("offset");
    if ( info == def.end() ) offset_ = 1;
    else offset_ =   atof(info->second.c_str());
}

void ParamDef::print(ostream& out) const
{
    out << "ParamDef[";
    out << "code=" << code_;
    out << ", long title=" <<  longTitle_;
    out << ", short title=" <<  shortTitle_;
    out << ", original units=" <<  originalUnit_;
    out << ", derived units=" <<  derivedUnit_;
    out << ", scaling factor=" <<  scaling_;
    out << ", offset=" <<  offset_;
    out << "]";
}
