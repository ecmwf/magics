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

/*! \file PaperDimension.h
    \brief Definition of the Template class PaperDimension.
    
    Magics Team - ECMWF 2005
    
    Started: Tue 14-Jun-2005
    
    Changes:
    
*/

#ifndef PaperDimension_H
#define PaperDimension_H

#include "magics.h"
#include "Factory.h"
#include "MagTranslator.h"

namespace magics {

class PaperDimension  {

public:
	PaperDimension() {}
	virtual ~PaperDimension() {}
    void setOrientation(string orientation) { landscape_ = magCompare(orientation, "landscape"); } 
    
    
    double getWidth()  { return (landscape_) ? largeDimension_ : smallDimension_;  }
    double getHeight() { return (landscape_) ? smallDimension_ : largeDimension_; }	
    
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const = 0; 
	 
	 
	 bool landscape_;
	 double smallDimension_;
	 double largeDimension_;

private:
    //! Copy constructor - No copy allowed
	PaperDimension(const PaperDimension&);
    //! Overloaded << operator to copy - No copy allowed
	PaperDimension& operator=(const PaperDimension&);

// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const PaperDimension& p)
		{ p.print(s); return s; }

};

class A6 : public PaperDimension
{
public:
	A6() { smallDimension_ = 10.5; largeDimension_ = 14.8; }
	~A6() {} 
protected: 
	virtual void print(ostream& out ) const { out << "format=a6[10.5,29.7]"; } 
};

class A5 : public PaperDimension
{
public:
	A5() { smallDimension_ = 14.8; largeDimension_ = 21.; }
	~A5() {} 
protected: 
	virtual void print(ostream& out ) const { out << "format=a5[14.8,21.]"; } 
};

class A4 : public PaperDimension
{
public:
	A4() { smallDimension_ = 21.; largeDimension_ = 29.7; }
	~A4() {} 
protected: 
	virtual void print(ostream& out ) const { out << "format=a4[21.,29.7]"; } 
};

class A3 : public PaperDimension
{
public:
	 A3() { smallDimension_ = 29.7; largeDimension_ = 42.; }
	~A3() {} 
protected: 
	virtual void print(ostream& out ) const { out << "format=a3[29.7,42.]"; } 
};

template<>
class MagTranslator<string, PaperDimension> { 
public:
	PaperDimension* operator()(const string& val )
	{
		return SimpleObjectMaker<PaperDimension>::create(val);
	}     

	PaperDimension* magics(const string& param)
	{
		PaperDimension* object;
		ParameterManager::update(param, object);
		return object;
	}
};


} // namespace magics
#endif
