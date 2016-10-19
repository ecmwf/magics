/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Factory.cc
    \brief Definition of Parameter base class.
    
    Magics Team - ECMWF 2004
    
    Started: Jan 2004
    
    Changes:
    
*/

#include "MagLog.h"
using namespace magics;
using namespace std;   // to run on AIX

template<class B>
map<string, SimpleFactory<B>* >* SimpleFactory<B>::map_ = 0;





template<class B>
SimpleFactory<B>::SimpleFactory(const string& name): name_(lowerCase(name))
{
	if (!map_) map_ = new map<string, SimpleFactory<B>* >();
	(*map_)[name_] = this; 
}


template<class B>
SimpleFactory<B>::~SimpleFactory()
{
	if (map_) {
		delete map_;
		map_ = 0;
	}
}

template<class B>
B* SimpleFactory<B>::create(const string& name)
{
	SimpleFactory<B>* maker = get(name);
	if (maker) {
		B* object =(*maker).make();
		return object;
	}
#ifdef MAGICS_EXCEPTION
	throw NoFactoryException(name);
#else 
    MagLog::info() << "SimpleFactory: Failed to create an object named '" << name << "'" << endl;
	return 0;
#endif	
} 



	
template<class B>
SimpleFactory<B>* SimpleFactory<B>::get(const string& name)
{	 
	ASSERT(map_);
	typename map<string, SimpleFactory<B>*>::iterator maker = (*map_).find(lowerCase(name));
	if (maker != (*map_).end()) return  (*maker).second;
#ifdef MAGICS_EXCEPTION
	throw NoFactoryException(name);
#else 
    MagLog::info() << "SimpleFactory: Failed to get an object named '" << name << "'" << endl;
	return 0;
#endif
}

	

