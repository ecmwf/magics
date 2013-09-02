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

/*! \file MetgramStyle.h
    \brief Definition of the Template class MetgramStyle.
    
    Magics Team - ECMWF 2006
    
    Started: Mon 16-Oct-2006
    
    Changes:
    
*/

#ifndef MetgramStyle_H
#define MetgramStyle_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "CustomisedPoint.h"
#include "PaperPoint.h"
#include "BasicGraphicsObject.h"
#include "MetgramFlagsAttributes.h"
#include "MetgramCurveAttributes.h"
namespace magics {


class LegendVisitor;
class BasicSceneObject;

class MetgramStyle {

public:
	MetgramStyle();
	virtual ~MetgramStyle();
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string&) {
        return false;
    }
    virtual MetgramStyle* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new MetgramStyle();
    }
        virtual void operator()(CustomisedPointsList&, BasicGraphicsObjectContainer&) {}

	virtual void toxml(ostream&) const {
   	
    } 
    virtual void visit(LegendVisitor&) {}
 
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	MetgramStyle(const MetgramStyle&);
    //! Overloaded << operator to copy - No copy allowed
	MetgramStyle& operator=(const MetgramStyle&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MetgramStyle& p)
		{ p.print(s); return s; }

};

class MetgramBar: public MetgramStyle {

public:
	MetgramBar() {}
	virtual ~MetgramBar() {}
    
    virtual void set(const XmlNode&) {
        MagLog::dev() << "(const XmlNode&)---> to be checked!...\n";
    }
    virtual void set(const map<string, string>&) {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
    }
    virtual bool accept(const string& node) {
        return magCompare(node, "bar");
    }
    virtual MetgramStyle* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new MetgramBar();
    }
    
    virtual void operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& task);
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

};

class MetgramCurve: public MetgramStyle,public MetgramCurveAttributes {

public:
	MetgramCurve() {}
	virtual ~MetgramCurve() {}
    
    virtual void set(const XmlNode& node) {
    	MetgramCurveAttributes::set(node);
    }
    virtual void set(const map<string, string>& map) {
    	MetgramCurveAttributes::set(map);
    }
    virtual MetgramStyle* clone() const {
        return new MetgramCurve();
    }
     virtual bool accept(const string& node) {
        return magCompare(node, "curve");
    }
    virtual void operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& task);
    virtual void visit(LegendVisitor&);
    virtual void set(const PaperPoint&, BasicGraphicsObjectContainer&) {}
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

};


class MetgramFlags: public MetgramStyle, public MetgramFlagsAttributes
{
public:
	MetgramFlags() {}
	virtual ~MetgramFlags() {}
    
    virtual void set(const XmlNode& node) {
        MetgramFlagsAttributes::set(node);
    }
    virtual void set(const map<string, string>& node) {
        MetgramFlagsAttributes::set(node);
    }
     virtual bool accept(const string& node) {
        return magCompare(node, "flags");
        
    }
    virtual MetgramStyle* clone() const {
        MagLog::dev() << "(const map<string, string&)---> to be checked!...\n";
        return new MetgramFlags();
    }
    
    virtual void operator()(CustomisedPointsList& points, BasicGraphicsObjectContainer& task);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

};

template <>
class MagTranslator<string, MetgramStyle> { 
public:
	MetgramStyle* operator()(const string& val )
	{
		return SimpleObjectMaker<MetgramStyle>::create(val);
	}     

	MetgramStyle* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};

} // namespace magics
#endif
