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

/*! \file HistoMode.h
    \brief Definition of the Template class HistoMode.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 18-May-2004
    
    Changes:
    
*/

#ifndef HistoMode_H
#define HistoMode_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"
#include "VectorOfPointers.h"
#include "BasicGraphicsObject.h"


namespace magics {


class Layout;

class HistoMode {

public:
	HistoMode();
	virtual ~HistoMode();
    
    virtual void set(const map<string, string>&) = 0; 
    virtual void set(const XmlNode&) = 0; 
    virtual HistoMode* clone() const = 0;
     virtual void toxml(ostream&, int)  const {}
    virtual void count(double, double) {}
    virtual void setToFirst(Layout&) {}
    virtual BasicGraphicsObject* next()  { return *(current_++); }
    virtual bool more()     		    { return current_ != objects_.end(); }
    string button(int);

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
     
    VectorOfPointers<vector<BasicGraphicsObject*> >			    objects_;
    VectorOfPointers<vector<BasicGraphicsObject*> >::iterator	current_;
    static int count_;

private:
    //! Copy constructor - No copy allowed
	HistoMode(const HistoMode&);
    //! Overloaded << operator to copy - No copy allowed
	HistoMode& operator=(const HistoMode&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const HistoMode& p)
		{ p.print(s); return s; }

};

template<>
class MagTranslator<string, HistoMode> { 
public:
	HistoMode* operator()(const string& val ) {
		 return SimpleObjectMaker<HistoMode>::create(val);
	}     
    HistoMode* magics(const string& param)
    {
        HistoMode* object;
		ParameterManager::update(param, object);
		return object;
    }
};


} // namespace magics
#endif
