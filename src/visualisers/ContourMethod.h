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

/*! \file ContourMethod.h
    \brief Definition of the Template class ContourMethod.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 11-Mar-2004
    
    Changes:
    
*/

#ifndef ContourMethod_H
#define ContourMethod_H

#include "magics.h"
#include "MatrixHandler.h"
#include "PointsHandler.h"

namespace magics {
    
class BasicGraphicsObjectContainer;
class XmlNode;


class ContourMethod  {

public:
	ContourMethod() {}
	virtual ~ContourMethod() {}
	virtual void set(const map<string, string>&) {}
	virtual void set(const XmlNode&) {}
	virtual bool accept(const string&) { return false; }
	void toxml(ostream&)  const {}

	virtual ContourMethod* clone()  const { return new ContourMethod(); }
	virtual MatrixHandler* handler(const AbstractMatrix& matrix, const BasicGraphicsObjectContainer&)
		{ return new MatrixHandler(matrix); }
	virtual bool needPoints() { return false; }
	virtual MatrixHandler* handlePoints(const AbstractPoints&, const Layout&)
		{ throw MethodNotYetImplemented("ContourMethod<P>::handler(const AbstractPoints<P>& matrix, const Layout&)"); }

protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const { out << "ContourMethod[]"; } 

private:
    //! Copy constructor - No copy allowed
	ContourMethod(const ContourMethod&);
    //! Overloaded << operator to copy - No copy allowed
	ContourMethod& operator=(const ContourMethod&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ContourMethod& p)
		{ p.print(s); return s; }

};

template <>
class MagTranslator<string, ContourMethod > {
public:
	ContourMethod* operator()(const string& val )
	{
		 return SimpleObjectMaker<ContourMethod >::create(val);
	}

	ContourMethod* magics(const char* param)
	{
		ContourMethod* object=0;
		ParameterManager::update(param, object);
		return object;
	}
	ContourMethod* magics(const string& param)
	{
	        ContourMethod* object=0;
		ParameterManager::update(param, object);
		return object;
	}

};
} // namespace magics

#endif
