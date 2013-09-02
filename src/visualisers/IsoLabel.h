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

/*! \file IsoLabel.h
    \brief Definition of the Template class IsoLabel.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 9-Mar-2004
    
    Changes:
    
*/

#ifndef IsoLabel_H
#define IsoLabel_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "LevelSelection.h"
#include "IsoLabelAttributes.h"

namespace magics {

class NoIsoLabel {

public:
	NoIsoLabel() {}
	virtual ~NoIsoLabel() {}
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual void toxml(ostream&, int = 0) const {}
	virtual bool accept(const string&) { return true;}
	
	virtual NoIsoLabel* clone() const {
		NoIsoLabel* object = new NoIsoLabel();	
		return object;
	}
	virtual void operator()(Polyline&, double) const; 
	virtual void prepare(const LevelSelection&) {}
	virtual void prepare(const LevelSelection&, const string&) { }
	virtual bool label() { return false; }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const { out << "NoIsoLabel[]"; } 
    
	// -- Friends
	    //! Overloaded << operator to call print().
	    friend ostream& operator<<(ostream& s,const  NoIsoLabel& p)
	        { p.print(s); return s; }
}; 

class IsoLabel: public NoIsoLabel, public map<double, double>, public IsoLabelAttributes {

public:
   IsoLabel();
   virtual ~IsoLabel();

   virtual IsoLabel* clone() const {
     IsoLabel* object = new IsoLabel();
     object->copy(*this);
     return object;
   }
   virtual void set(const map<string, string>& map) {  IsoLabelAttributes::set(map); }
   	virtual void set(const XmlNode& node){  IsoLabelAttributes::set(node); }

   	virtual bool accept(const string& verb) { return  IsoLabelAttributes::accept(verb); }
   virtual void operator()(Polyline&, double) const;
   virtual bool label() { return true; }
   virtual void prepare(const LevelSelection& levels, const string& colour)
   {
//        int label = 0;


        // first we find the reference ...
        vector<double> todo;
        levels.thinLevels(frequency_, todo);
        for (LevelSelection::const_iterator level = todo.begin(); level != todo.end(); ++level) {
                (*this)[*level] = *level;
        }
    }

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

    typedef string (IsoLabel::*Method)(double) const;
    std::map<string, Method> methods_;
    string number(double) const;
    string text(double) const;
    string both(double) const;

private:
    //! Copy constructor - No copy allowed
    IsoLabel(const IsoLabel&);
    //! Overloaded << operator to copy - No copy allowed
    IsoLabel& operator=(const IsoLabel&);
};




template<>
class MagTranslator<string, NoIsoLabel> {
public:
    NoIsoLabel* operator()(const string& val ) {
       return SimpleObjectMaker<NoIsoLabel>::create(val);
    }
    NoIsoLabel* magics(const string& param)
    {
		NoIsoLabel* object=0;
		ParameterManager::update(param, object);
		return object;
    }

};
} // namespace magics
#endif
