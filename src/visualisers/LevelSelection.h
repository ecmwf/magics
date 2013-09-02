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

/*! \file LevelSelection.h
    \brief Definition of the Template class LevelSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
    
*/

#ifndef LevelSelection_H
#define LevelSelection_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "LevelSelectionAttributes.h"
#include "Data.h"

namespace magics {

class UserPoint;
class PointsHandler;

class LevelSelectionInterface 
{
public:
    virtual int getCount() const = 0; 
    virtual int getTolerance() const = 0;
    virtual double getReference() const = 0;
    virtual double getInterval() const = 0;
    virtual doublearray getList() const = 0;
    virtual double getMin() const = 0;
    virtual double getMax() const = 0;
};

class LevelSelection: public LevelSelectionAttributes, public doublearray
{
public:
	LevelSelection();
	virtual ~LevelSelection();
	
	virtual LevelSelection* clone() const {

    	return 0;
    } 
    virtual void set(const XmlNode& node)          { LevelSelectionAttributes::set(node); }
    virtual void set(const map<string,string>& map) { LevelSelectionAttributes::set(map); }
    virtual void set(const LevelSelectionInterface&) {}
    
	virtual void calculate(double, double, bool) {};
	virtual double reference() const;
	virtual void thinLevels(int frequency, vector<double>&) const;
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 

private:
	//! Copy constructor - No copy allowed
	LevelSelection(const LevelSelection&);
	//! Overloaded << operator to copy - No copy allowed
	LevelSelection& operator=(const LevelSelection&);
    
// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const LevelSelection& p)
		{ p.print(s); return s; }
};

template<>
class MagTranslator<string, LevelSelection> { 
public:
	LevelSelection* operator()(const string& val )
	{
		 return SimpleObjectMaker<LevelSelection>::create(val);
	}     
	LevelSelection* magics(const string& param)
	{
		LevelSelection* object=0;
		ParameterManager::update(param, object);
		return object;
	}

};
} // namespace magics
#endif
