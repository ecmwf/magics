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

#ifndef HeightTechnique_H
#define HeightTechnique_H

#include "magics.h"


#include "MagTranslator.h"
#include "Factory.h"
#include "IntervalMap.h"

namespace magics {

class XmlNode;
class LevelSelection;

class HeightTechniqueInterface
{
public: 
    HeightTechniqueInterface() {}
    virtual ~HeightTechniqueInterface() {}
	virtual double getMinHeight() const = 0;
	virtual double getMaxHeight() const = 0;
	virtual floatarray getHeights() const = 0;
	virtual ListPolicy getHeightPolicy() const = 0;
	
};

class HeightTechnique: public vector<float> {

public:
	HeightTechnique();
	virtual ~HeightTechnique();
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    
    virtual HeightTechnique* clone() const { return new HeightTechnique(); }
    
    void toxml(ostream&)  const {}
	
	virtual void set(const HeightTechniqueInterface&); 
	virtual void prepare(LevelSelection&) {}
	double height(double val) { return heights_.find(val, 0.2); }
   

protected:
     //! Method to print  about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	  IntervalMap<double> heights_;

private:
    //! Copy constructor - No copy allowed
	HeightTechnique(const HeightTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	HeightTechnique& operator=(const HeightTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HeightTechnique& p)
		{ p.print(s); return s; }
};

class ListHeightTechnique : public HeightTechnique
{
public:
	ListHeightTechnique();
	virtual ~ListHeightTechnique();
	virtual void prepare(LevelSelection&);
    virtual void set(const HeightTechniqueInterface&); 
protected:
	vector<double> list_;
	ListPolicy policy_;
};

class CalculateHeightTechnique: public HeightTechnique
{
public:
	CalculateHeightTechnique();
	virtual ~CalculateHeightTechnique();
	virtual void prepare(LevelSelection&);
    virtual void set(const HeightTechniqueInterface&); 
protected:
	double min_;
	double max_;
	
};



template<>
class MagTranslator<string, HeightTechnique> { 
public:
	HeightTechnique* operator()(const string& val )
	{
		return SimpleObjectMaker<HeightTechnique>::create(val);
	}     

	HeightTechnique* magics(const string& param)
	{
		HeightTechnique* object;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
