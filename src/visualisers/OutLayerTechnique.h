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

/*! \file HeightSelection.h
    \brief Definition of the Template class HeightSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/

#ifndef OutLayerTechnique_H
#define OutLayerTechnique_H

#include "magics.h"


#include "MagTranslator.h"
#include "Factory.h"

namespace magics {

class XmlNode;

class OutLayerTechniqueInterface
{
public: 
    OutLayerTechniqueInterface() {}
    virtual ~OutLayerTechniqueInterface() {}
	virtual float getMinOutlayer() const = 0;
	virtual float getMaxOutlayer() const = 0;
	
};

class NoOutLayerTechnique {

public:
	NoOutLayerTechnique();
	virtual ~NoOutLayerTechnique();
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    
    virtual NoOutLayerTechnique* clone() const { return new NoOutLayerTechnique(); }
    
    void toxml(ostream&)  const {}
	
	
	
   

protected:
     //! Method to print  about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 

private:
    //! Copy constructor - No copy allowed
	NoOutLayerTechnique(const NoOutLayerTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	NoOutLayerTechnique& operator=(const NoOutLayerTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const NoOutLayerTechnique& p)
		{ p.print(s); return s; }
};

class SimpleOutLayerTechnique : public NoOutLayerTechnique
{
public:
	SimpleOutLayerTechnique();
	virtual ~SimpleOutLayerTechnique();
  
	
};





template<>
class MagTranslator<string, NoOutLayerTechnique> { 
public:
	NoOutLayerTechnique* operator()(const string& val )
	{
		return SimpleObjectMaker<NoOutLayerTechnique>::create(val);
	}     

	NoOutLayerTechnique* magics(const string& param)
	{
		NoOutLayerTechnique* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
