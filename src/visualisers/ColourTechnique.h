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

/*! \file ColourTechnique.h
    \brief Definition of the Template class ColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#ifndef ColourTechnique_H
#define ColourTechnique_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "ColourTable.h"
#include "IntervalMap.h"


namespace magics {

class LevelSelection;
class LegendVisitor;

struct ColourInfo 
{
    ColourInfo() :  index_(-1), level_(0), left_("NONE"), right_("NONE") {}
    ColourInfo(int index, double level, const Colour& left, const Colour& right) :
        index_(index), level_(level), left_(left), right_(right) {}
    
    int      index_;
    double level_;
    Colour   left_;
    Colour   right_;
};


class ColourTechniqueInterface 
{
public:
    virtual const Colour&  getMinColour() const = 0; 
    virtual const Colour&  getMaxColour() const = 0;
    virtual const string& getDirection() const = 0;
    virtual stringarray getColours() const = 0;
    virtual ListPolicy getPolicy() const = 0;
};




class ColourTechnique : public map<double, ColourInfo>  {

public:
	ColourTechnique();
	virtual ~ColourTechnique();
    virtual void set(const map<string, string>&) {}
    virtual void set(const XmlNode&) {}
    virtual bool accept(const string&) { return false; }
    virtual ColourTechnique* clone() const { return new ColourTechnique(); }
    void toxml(ostream&)  const {}
    virtual void set(const ColourTechniqueInterface&) {};
    
    Colour operator()(double) const;
    Colour left(double) const;
    Colour right(double) const;
    Colour colour(double) const;
    double leftRange(double) const;
    double rightRange(double) const;
    
    virtual void set(ColourTable&, int) const {}
    void prepare(const LevelSelection&, bool rainbow = false);
    
    void colours(vector<string>&) const; 
    
	void visit(LegendVisitor&);
	ListPolicy getPolicy() const { return policy_; }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     IntervalMap<Colour> bands_;
     
     map<double, pair<double, double> > ranges_;
     double maxLevel_;
     
     ListPolicy policy_;

     




    
     
private:
    //! Copy constructor - No copy allowed
	ColourTechnique(const ColourTechnique&);
    //! Overloaded << operator to copy - No copy allowed
	ColourTechnique& operator=(const ColourTechnique&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ColourTechnique& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, ColourTechnique> { 
public:
	ColourTechnique* operator()(const string& val ) {
		 return SimpleObjectMaker<ColourTechnique>::create(val);
	}     
    ColourTechnique* magics(const string& param)
    {
        ColourTechnique* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};


} // namespace magics
#endif
