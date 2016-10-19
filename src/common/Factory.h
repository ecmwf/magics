/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
