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
    \brief Definition of the Template class LocalTable.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/

#ifndef LocalTable_H
#define LocalTable_H

#include "magics.h"

#include "DefinitionTable.h"


namespace magics {


class ParamDef 
{
public :
    ParamDef(const map<string, string>& def);
    ParamDef(): code_(-1), 
        longTitle_("unknown parameter"), 
        shortTitle_("unknown parameter"),
        originalUnit_("unknown units"),
        derivedUnit_("unknown units"),
        scaling_(1),
        offset_(0) {}
    virtual ~ParamDef() {}
    int code()                  { return code_; }
    string longTitle() const    { return longTitle_; }
    string shortTitle() const   { return shortTitle_; }
    string originalUnit() const { return originalUnit_; }
    string derivedUnit() const  { return derivedUnit_; }
    double scaling() const       { return scaling_; }
    double offset() const        { return offset_; }
    
protected:
  //! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
    
    int    code_;
    string longTitle_;
    string shortTitle_;
    string originalUnit_;
    string derivedUnit_;
    double  scaling_;
    double  offset_;   
    
    // -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ParamDef& p)
		{ p.print(s); return s; }
};


class LocalTable : public DefinitionTable<ParamDef>
{
public:
	LocalTable(const string&);
	virtual ~LocalTable();
    static const DefinitionTable<ParamDef>& localTable(long table, long center = -1);
    static const ParamDef& localInfo(long code, long table, long center = -1);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     void setDefinition() { definition_ = "param"; }
   
 
private:
    //! Copy constructor - No copy allowed
	LocalTable(const LocalTable&);
    //! Overloaded << operator to copy - No copy allowed
	LocalTable& operator=(const LocalTable&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LocalTable& p)
		{ p.print(s); return s; }

};

} // namespace magics
#endif
