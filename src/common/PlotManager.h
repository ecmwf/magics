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

/*! \file PlotManager.h
    \brief Definition of the Template class PlotManager.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Aug-2004
    
    Changes:
    
*/

#ifndef PlotManager_H
#define PlotManager_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "MagicsManager.h"
#include "Node.h"

namespace magics {

class XmlNode;

class PlotManager : public stack<BaseSceneObject*> {

public:
	PlotManager();
	virtual ~PlotManager();
    
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual PlotManager* clone() { return new PlotManager(); }
    void toxml(ostream&, int)  const {}
    
    virtual void superpage(MagicsManager&);
    virtual void page(MagicsManager&);
    virtual void subpage(MagicsManager&);
    virtual void check(MagicsManager&);
    virtual void addpage(MagicsManager&);
    void addNode(MagicsManager&, BaseSceneObject* object);
    void add(BaseSceneObject* object);

    virtual void addRoot(MagicsManager&);
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     bool page_;

private:
    //! Copy constructor - No copy allowed
	PlotManager(const PlotManager&);
    //! Overloaded << operator to copy - No copy allowed
	PlotManager& operator=(const PlotManager&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PlotManager& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, PlotManager> { 
public:
	PlotManager* operator()(const string& val )
	{
		return SimpleObjectMaker<PlotManager>::create(val);
	}     

	PlotManager* magics(const string& param)
	{
		PlotManager* object;
		ParameterManager::update(param, object);
		return object;
	}
};


} // namespace magics
#endif
