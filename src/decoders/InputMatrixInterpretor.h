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

/*! \file InputMatrixInterpretor.h
    \brief Definition of the Template class InputMatrixInterpretor.
    
    Magics Team - ECMWF 2005
    
    Started: Fri 16-Sep-2005
    
    Changes:
    
*/

#ifndef InputMatrixInterpretor_H
#define InputMatrixInterpretor_H

#include "magics.h"
#include "MagTranslator.h"
#include "Factory.h"
#include "InputMatrixRegularInterpretorAttributes.h"
#include "InputMatrixIrregularInterpretorAttributes.h"


namespace magics {

class XmlNode;
class UserPoint;
class UserPoint;
class Transformation;


class InputMatrix;

class InputMatrixInterpretor {

public:
	InputMatrixInterpretor();
	virtual ~InputMatrixInterpretor();
	
	virtual InputMatrixInterpretor* clone() { return new InputMatrixInterpretor(); }
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual void toxml(ostream&)  const {}
	virtual bool accept(const string&) { return false; }
	virtual Matrix* xyInterpret(Matrix*, const InputMatrix&) { return 0; }
	virtual Matrix* geoInterpret(Matrix*, const InputMatrix&) { return 0; }
	virtual void getReady(const Transformation&) { }

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	typedef void (InputMatrixInterpretor::*Mapper)(); 
	std::map<string, Mapper> mappers_;
	void upperLeft();
	void lowerLeft();
	void upperRight();
	void lowerRight();     
	void upperLeftTransposed();
	void lowerLeftTransposed();
	void upperRightTransposed();
	void lowerRightTransposed();

private:
	//! Copy constructor - No copy allowed
	InputMatrixInterpretor(const InputMatrixInterpretor&);
	//! Overloaded << operator to copy - No copy allowed
	InputMatrixInterpretor& operator=(const InputMatrixInterpretor&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const InputMatrixInterpretor& p)
		{ p.print(s); return s; }
};


class InputMatrixRegularInterpretor: public InputMatrixRegularInterpretorAttributes, public InputMatrixInterpretor
{
public:
	InputMatrixRegularInterpretor();
	virtual ~InputMatrixRegularInterpretor();

	virtual void set(const map<string, string>& map) { InputMatrixRegularInterpretorAttributes::set(map); }
	virtual void set(const XmlNode& node) { InputMatrixRegularInterpretorAttributes::set(node); }
    
	virtual Matrix* xyInterpret(Matrix*, const InputMatrix&);
	virtual Matrix* geoInterpret(Matrix*, const InputMatrix&);
    virtual void getReady(const Transformation&);
protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const; 
	 string dateX_; // Used to adjust the Matrix according to the projection
	 string dateY_; // Used to adjust the Matrix according to the projection

private:
	//! Copy constructor - No copy allowed
	InputMatrixRegularInterpretor(const InputMatrixRegularInterpretor&);
	//! Overloaded << operator to copy - No copy allowed
	InputMatrixRegularInterpretor& operator=(const InputMatrixRegularInterpretor&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const InputMatrixRegularInterpretor& p)
		{ p.print(s); return s; }
};

class InputMatrixGaussianInterpretor : public InputMatrixRegularInterpretor
{
public:
	InputMatrixGaussianInterpretor() {}
	virtual ~InputMatrixGaussianInterpretor() {}

};

class InputMatrixIrregularInterpretor: public InputMatrixIrregularInterpretorAttributes, public InputMatrixInterpretor {

public:
	InputMatrixIrregularInterpretor();
	virtual ~InputMatrixIrregularInterpretor();
	
	virtual void set(const map<string, string>& map) { InputMatrixIrregularInterpretorAttributes::set(map); }
	virtual void set(const XmlNode& node) { InputMatrixIrregularInterpretorAttributes::set(node); }
    
	virtual Matrix* geoInterpret(Matrix*, const InputMatrix&);
	virtual Matrix* xyInterpret(Matrix*, const InputMatrix&);

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const;

private:
	//! Copy constructor - No copy allowed
	InputMatrixIrregularInterpretor(const InputMatrixIrregularInterpretor&);
	//! Overloaded << operator to copy - No copy allowed
	InputMatrixIrregularInterpretor& operator=(const InputMatrixIrregularInterpretor&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const InputMatrixIrregularInterpretor& p)
		{ p.print(s); return s; }
};


template<>
class MagTranslator<string, InputMatrixInterpretor> { 
public:
	InputMatrixInterpretor* operator()(const string& val )
	{
		return SimpleObjectMaker<InputMatrixInterpretor>::create(val);
	}     

	InputMatrixInterpretor* magics(const string& param)
	{
		InputMatrixInterpretor* object=0;
		ParameterManager::update(param, object);
		return object;
	}

};

} // namespace magics
#endif
