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

#ifndef DefinitionTable_H
#define DefinitionTable_H

#include "magics.h"

namespace magics {
 
class BaseTable 
{
public:
    BaseTable(const string& definition) : definition_(definition) {}
    virtual ~BaseTable() {}
    const string& info() const { return definition_; }
    string definition_;
    virtual void add(const map<string, string>&) = 0;
   
};

template <class D>
class DefinitionTable : public BaseTable, public map<int, D*>
{
public:
	DefinitionTable(const string&, const string&);
	virtual ~DefinitionTable();
	 virtual void toxml(ostream&, int)  const {}
    
   
   
    const D& definition(int code) const {
        typename map<int, D*>::const_iterator param = map<int, D*>::find(code);
        if (param == this->end() ) return unknown_;
        return *(param->second);
    }
    
    static const DefinitionTable<D>& definitionTable(const string&, const string&);
    static const D& definitionInfo(const string&, const string&, int code);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     string definition_;
     
     void add(const map<string, string>& def) 
     {
        D* param = new D(def);
        (*this)[param->code()] = param;
     } 
     
     static map<string, DefinitionTable<D>* >* tables_;
     static D unknown_;
   
 
private:
    //! Copy constructor - No copy allowed
	DefinitionTable(const DefinitionTable<D>&);
    //! Overloaded << operator to copy - No copy allowed
	DefinitionTable& operator=(const DefinitionTable<D>&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const DefinitionTable<D>& p)
		{ p.print(s); return s; }

};

#include "DefinitionTable.hcc"
} // namespace magics
#endif
