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
/*! \file TePrototype.h
    \brief This file contains a template for the "TePrototype" pattern.
*/
#ifndef TePrototype_H
#define TePrototype_H
//!  Implements a template for the "Prototype" pattern.
/*
	See the book "Design Patterns - Elements of Reusable Object-Oriented Software", by Gamma et. al.
	for further documentation.
*/

#include "TeDefines.h"

template <class T>
class TePrototype
{
public:

	//! Export pointer to the structure
	typedef  TePrototype<T>* TePrototypePtr;

// -- Normal Constructor

	//! Constructor
	TePrototype ();
	
	//! Destructor
	virtual ~TePrototype() {}

	//! Virtual Constructor 
	static T* clone ();

	//! Builds a new type (should be implemented by descendants)
    virtual T* build () = 0;

private:
	static TePrototypePtr instance_;
};

// Initialisation of static variable
template <class T>
typename TePrototype<T>::TePrototypePtr TePrototype<T>::instance_;

// Constructor
template <class T>
TePrototype<T>::TePrototype()
{
	// Put the object in the factory dictionary
	instance_ = this;
}

// Virtual Constructor
template <class T> 
T*
TePrototype<T>::clone ()
{
	return instance_->build();
}

#endif

