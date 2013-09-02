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

/*! \file AxisGrid.h
    \brief Definition of the Template class AxisGrid.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 7-May-2004
    
    Changes:
    
*/

#ifndef AxisGrid_H
#define AxisGrid_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"

#include "AxisGridAttributes.h"
#include "AxisItem.h"


namespace magics {


class XmlNode;
class Transformation; 
class DrawingVisitor;
class HorizontalAxisVisitor;
class VerticalAxisVisitor;

class AxisGrid: public AxisGridAttributes {

public:
	AxisGrid();
	virtual ~AxisGrid();
	
	void set(const map<string, string>& map) { AxisGridAttributes::set(map); }
	void set(const XmlNode& node) { AxisGridAttributes::set(node); }

	AxisGrid* clone()
	{
		AxisGrid* grid = new AxisGrid();
		grid->copy(*this);
		return grid;
	}

    virtual void vertical(const AxisItems&, DrawingVisitor& out) const;
	virtual void horizontal(const AxisItems&,  DrawingVisitor& out) const;
	virtual void  vertical(const AxisItems&, HorizontalAxisVisitor&) const {}
	virtual void  vertical(const AxisItems&, VerticalAxisVisitor&) const {}	
	virtual void  horizontal(const AxisItems&, HorizontalAxisVisitor&) const {}
	virtual void  horizontal(const AxisItems&, VerticalAxisVisitor&) const {}

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	AxisGrid(const AxisGrid&);
    //! Overloaded << operator to copy - No copy allowed
	AxisGrid& operator=(const AxisGrid&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const AxisGrid& p)
		{ p.print(s); return s; }

};

class NoAxisGrid : public AxisGrid
{
public :
	NoAxisGrid() {}
	~NoAxisGrid() {}

	virtual void vertical(const AxisItems&, DrawingVisitor& ) const {}
	virtual void horizontal(const AxisItems&,  DrawingVisitor& ) const {}

	AxisGrid* clone()
	{
		AxisGrid* grid = new NoAxisGrid();
		return grid;
	}
};


template<>
class MagTranslator<string, AxisGrid> { 
public:
	AxisGrid* operator()(const string& val )
	{
		return SimpleObjectMaker<AxisGrid>::create(val);
	}

	AxisGrid* magics(const string& param)
	{
		AxisGrid* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
