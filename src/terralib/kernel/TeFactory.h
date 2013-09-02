/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright  2001-2007 INPE and Tecgraf/PUC-Rio.

This code is part of the TerraLib library.
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

You should have received a copy of the GNU Lesser General Public
License along with this library.

The authors reassure the license terms regarding the warranties.
They specifically disclaim any warranties, including, but not limited to,
the implied warranties of merchantability and fitness for a particular purpose.
The library provided hereunder is on an "as is" basis, and the authors have no
obligation to provide maintenance, support, updates, enhancements, or modifications.
In no event shall INPE and Tecgraf / PUC-Rio be held liable to any party for direct,
indirect, special, incidental, or consequential damages arising out of the use
of this library and its documentation.
*************************************************************************************/
/*! \file TeFactory.h
    \brief This file deals with the Factory Pattern 
*/
#ifndef  __TERRALIB_INTERNAL_FACTORY_H
#define  __TERRALIB_INTERNAL_FACTORY_H

#include <map>
#include <string>

using namespace std;

//!  Implements a template for the "TeFactory" pattern.
/*
  The "factory" pattern is a technique for creating new
  instances of objects which defines an abstract interface,
  (represented by the "Make" module). The subclasses of
  factory decide which class to instantiate.
  

  Each subclass of factory "registers" itself at compile time;
  therefore, the addition of a new factory requires no change
  the parent class. 

  \author Gilberto Camara
*/
template <class T, class Arg>
class TeFactory
{
public:

//! Dictionary of factories (indexed by name)
	typedef map<string, TeFactory<T,Arg>* > TeFactoryMap; 

//! Returns the single instance of the factory dictionay
	static TeFactoryMap& instance ()
	{ 
		static TeFactoryMap Fmap_;
		return Fmap_;
		
	}
	
	//! Normal Constructor
	TeFactory (const string& factoryName);

	virtual ~TeFactory() {}

	//! Virtual constructor using name
	static T* make  ( string name, const Arg& arg );

	//! Virtual constructor using arguments
	static T* make  ( const Arg& arg  );


protected:

	//!  Builds a new type (should be implemented by descendants)
    virtual T* build ( const Arg& arg ) = 0;

private:
	string  Fname_;
};

// Initialisation of static variable
//template <class T, class Arg>
//TeFactory<T,Arg>::TeFactoryMap TeFactory<T,Arg>::Fmap_;


// Constructor
template <class T, class Arg>
TeFactory<T,Arg>::TeFactory(const string& name):
	Fname_(name)
{
	TeFactory<T,Arg>::instance()[name] = this;

}

// Destructor
//template <class T, class Arg>
//TeFactory<T,Arg>::~TeFactory<T,Arg> ()
//{
	// Remove the object from the factory dictionary
	// Fmap_.erase ( Fname_ );
//}

//! Builds an object, based on the input parameters
template <class T, class Arg> 
T*
TeFactory<T,Arg>::make ( string name, const Arg& arg )
{
	// try to find the name on the factory dictionary
	typename TeFactoryMap::iterator i = TeFactory<T,Arg>::instance().find ( name );

	// Not found ?  return the Default Object   
	if ( i == TeFactory<T,Arg>::instance().end() )
		return T::DefaultObject( arg );

	// Create an object, based on the input parameters
	return (*i).second->build ( arg );
	return 0;

}

//! Builds an object, based on the input parameters
template <class T, class Arg> 
T*
TeFactory<T,Arg>::make (const Arg& arg )
{
	string name = arg.decName();

	// try to find the name on the factory dictionary
	typename TeFactoryMap::iterator i = TeFactory<T,Arg>::instance().find ( name );

	// Not found ?  return the Default Object   
	if ( i == TeFactory<T,Arg>::instance().end() )
		return T::DefaultObject( arg );

	// Create an object, based on the input parameters
	return (*i).second->build ( arg );
	return 0;

}
#endif

