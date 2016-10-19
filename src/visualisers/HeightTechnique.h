/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
