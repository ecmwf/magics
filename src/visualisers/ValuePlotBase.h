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

/*! \file ValuePlotBase.h
    \brief Definition of the Template class ValuePlotBase.
    
    Magics Team - ECMWF 2006
    
    Started: Thu 9-Feb-2006
    
    Changes:
    
*/

#ifndef ValuePlotBase_H
#define ValuePlotBase_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"



namespace magics {

class XmlNode;
class MatrixHandler;
class BasicGraphicsObjectContainer;
class Data;
class LegendVisitor;

class ValuePlotBase {

public:
	ValuePlotBase() {}
	virtual ~ValuePlotBase() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "ValuePlotBase::set(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "ValuePlotBase::set(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) { return false; }

    virtual ValuePlotBase* clone() const { return new ValuePlotBase(); }
     virtual void toxml(ostream&, int = 0) const {
    	 MagLog::dev() << "ValuePlotBase::virtual void toxml(ostream&, int = 0) const ---> to be checked!...\n";
    } 
    
    virtual void operator()(MatrixHandler&, BasicGraphicsObjectContainer&) {}
    virtual void operator()(Data&, BasicGraphicsObjectContainer&) {}
    
    virtual void visit(LegendVisitor&) {
        MagLog::dev() << "ValuePlotBase::visit(LegendBase&)---> to be checked!...\n";
    }
    
    virtual string getType() { return "unknown"; }
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "ValuePlotBase\n"; } 

private:
    //! Copy constructor - No copy allowed
	ValuePlotBase(const ValuePlotBase&);
    //! Overloaded << operator to copy - No copy allowed
	ValuePlotBase& operator=(const ValuePlotBase&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ValuePlotBase& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, ValuePlotBase > {
public:
	ValuePlotBase* operator()(const string& val )
	{
		return SimpleObjectMaker<ValuePlotBase >::create(val);
	}     

	ValuePlotBase* magics(const string& param)
	{
		ValuePlotBase* object=0;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
