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

/*! \file Factory.h
    \brief Definition of Factory  class.
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/
#ifndef MPP_Factory_H
#define MPP_Factory_H


#include "magics.h"
#include "MagException.h" 
#include "MagLog.h"

namespace magics {

class NoFactoryException : public MagicsException
{
public:
	NoFactoryException( const string& factory ):
		MagicsException("Factory (" +  factory + ") not found") {}
	NoFactoryException():
		MagicsException("Factory not found") {}
}; 



template<class B> 
class SimpleFactory {
public:
// -- Contructors
	SimpleFactory(const string& name);
	virtual ~SimpleFactory();
// methods
	static B*  create(const string& name);
	virtual B* make() const  = 0;
// -- Members
	static map<string, SimpleFactory<B>* >* map_;
	static SimpleFactory<B>* get(const string& name);
	string name_;	 
};




template <class A, class B=A>
class SimpleObjectMaker : public SimpleFactory<B>
{
public :
	SimpleObjectMaker(const string& name) : SimpleFactory<B>(name) 
        {}
	B* make() const {  return new A(); }
};


} // namespace magics

#include "Factory.cc"

#endif
