/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GribTables.h
    \brief Definition of the Template class GribTables.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/

#ifndef GribTables_H
#define GribTables_H

#include "magics.h"

#include "DefinitionTable.h"


namespace magics {


class GeneralDef 
{
public :
    GeneralDef(const map<string, string>& def)
    {
            map<string, string>::const_iterator info;
    
            info = def.find("code");
            if ( info == def.end() ) code_ = -1;
            else code_ =  atoi(info->second.c_str());
            
            info = def.find("long_title");
            if ( info == def.end() ) longTitle_ = "Unknown";
            else longTitle_ =  info->second;
            
            info = def.find("short_title");
            if ( info == def.end() ) shortTitle_ =  "Unknown";
            else shortTitle_ =  info->second;
    }
    
    GeneralDef(): code_(-1), 
        longTitle_("unknown"), 
        shortTitle_("unknown") {}
    virtual ~GeneralDef() {}
    int code()                  { return code_; }
    string longTitle() const    { return longTitle_; }
    string shortTitle() const   { return shortTitle_; }
   
    
protected:
  //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream& out) const {
        out << "GeneralDef[";
        out << "code=" << code_;
        out << ", long title=" <<  longTitle_;
        out << ", short title=" <<  shortTitle_;
        out << "]";
	 }
    
    int    code_;
    string longTitle_;
    string shortTitle_;
  
    
    // -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const GeneralDef& p)
		{ p.print(s); return s; }
};

template <> 
map<string, DefinitionTable<GeneralDef>* >* DefinitionTable<GeneralDef>::tables_ = 0;
class CentreTable : public DefinitionTable<GeneralDef>
{
public:
	CentreTable() : DefinitionTable<GeneralDef>("centre.xml", "centre") {}
    
	virtual ~CentreTable() {}
    static const GeneralDef& definition(int code) { return definitionInfo("centre.xml", "centre", code); } 
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	
  
private:
    //! Copy constructor - No copy allowed
	CentreTable(const CentreTable&);
    //! Overloaded << operator to copy - No copy allowed
	CentreTable& operator=(const CentreTable&);


};

class LevelTable : public DefinitionTable<GeneralDef>
{
public:
	LevelTable() : DefinitionTable<GeneralDef>("level.xml", "level") {}
    
	virtual ~LevelTable() {}
    static const GeneralDef& definition(int code) { return definitionInfo("level.xml", "level", code); } 

	
 

};

class TypeTable : public DefinitionTable<GeneralDef>
{
public:
	TypeTable() : DefinitionTable<GeneralDef>("type.xml", "type") {}
    
	virtual ~TypeTable() {}
    static const GeneralDef& definition(int code) { return definitionInfo("type.xml", "type", code); } 
    

 

};
class SatelliteTable : public DefinitionTable<GeneralDef>
{
public:
	SatelliteTable() : DefinitionTable<GeneralDef>("satellite.xml", "identifier") {}
    
	virtual ~SatelliteTable() {}
    static const GeneralDef& definition(int code) { return definitionInfo("satellite.xml", "identifier", code); } 
    

 

};

} // namespace magics
#endif
