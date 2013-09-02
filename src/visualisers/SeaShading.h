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

/*! \file SeaShading.h
    \brief Definition of the Template class SeaShading.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 7-Mar-2005
    
    Changes:
    
*/

#ifndef SeaShading_H
#define SeaShading_H

#include "magics.h"
#include "SeaShadingAttributes.h"

#include "UserPoint.h"
#include "Polyline.h"

namespace magics {
	
class XmlNode;
class UserPoint;

class LayoutVisitor;


class NoSeaShading {

public:
	NoSeaShading() {}
	virtual ~NoSeaShading() {}

	virtual void set(const XmlNode&) {
	}
	virtual void set(const map<string, string>&) {
	}
	virtual bool accept(const string& ) { return false; }

	virtual void toxml(ostream&, int = 0) const {
	} 
	virtual NoSeaShading* clone() const {
		return new NoSeaShading();
	}
 	virtual Colour colour() { 
		return Colour("NONE"); 
	}
	virtual bool shading() { return false; } 
	virtual void sea(Polyline* poly,BasicGraphicsObjectContainer&) { delete poly; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "NoSeaShading\n"; } 

private:
    //! Copy constructor - No copy allowed
	NoSeaShading(const NoSeaShading&);
    //! Overloaded << operator to copy - No copy allowed
	NoSeaShading& operator=(const NoSeaShading&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoSeaShading& p)
		{ p.print(s); return s; }
};


template <>
class MagTranslator<string, NoSeaShading>
{
public:
	NoSeaShading* operator()(const string& val )
	{
		return SimpleObjectMaker<NoSeaShading>::create(val);
	}     

	NoSeaShading* magics(const string& param)
	{
		string val;
		ParameterManager::get(param, val);
		return (*this)(val);
	}

};


class SeaShading:  public NoSeaShading, public SeaShadingAttributes {

public:
	SeaShading();
	virtual ~SeaShading();
	virtual void set(const map<string, string>& map)
		{ SeaShadingAttributes::set(map); }
	virtual void set(const XmlNode& node)
		{ SeaShadingAttributes::set(node); }
	bool accept(const string& node) { return SeaShadingAttributes::accept(node); }
	virtual NoSeaShading* clone() const {
		SeaShading* object = new SeaShading();
		object->copy(*this);
		return object;
	}
	virtual bool shading() { return true; } 
	virtual Colour colour() { return *colour_; }
	virtual void sea(Polyline* sea, BasicGraphicsObjectContainer& visitor);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	SeaShading(const SeaShading&);
    //! Overloaded << operator to copy - No copy allowed
	SeaShading& operator=(const SeaShading&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const SeaShading& p)
		{ p.print(s); return s; }
};

/*
class NoSeaShading: public NoSeaShading {

public:
	NoSeaShading() {}
	virtual ~NoSeaShading() {}
	virtual NoSeaShading* clone() const {
    	NoSeaShading* object = new NoSeaShading();
    
    	return object;
    }
    virtual void prepare(GraphicsList&, PolylineSet<UserPoint>*) { }

	virtual Colour colour() { return Colour("NONE"); }
};
*/


} // namespace magics
#endif
