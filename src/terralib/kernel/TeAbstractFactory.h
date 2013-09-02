/************************************************************************************
TerraLib - a library for developing GIS applications.
Copyright © 2001-2007 INPE and Tecgraf/PUC-Rio.

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
/*! \file TeAbstractFactory.h
	\brief Implements a template for the Abstract Factory pattern
*/
#ifndef  __TERRALIB_INTERNAL_NEW_FACTORY_H
#define  __TERRALIB_INTERNAL_NEW_FACTORY_H

#include "TeDefines.h"
#include <map>
#include <string>

using std::map;
using std::string;

//!  Implements a template for the Abstract Factory pattern.
/*
    This file implements a templated version of the Abstract Factory Pattern 
	See: "Design Patterns - Elements of Reusable Object-Oriented Software", by Gamma et. al
	page 87.
  \author Gilberto Camara, Lúbia Vinhas
*/
template <typename PRODUCT,						// The product to be made
		  typename PARAMS,						// The parameter necessary to make the product
		  typename PRODUCTIDTYPE=std::string>	// The type used to identify the product
class TeAbstractFactory
{
public:

	//! To link each product to its factory
	typedef map<PRODUCTIDTYPE, TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>* > TeFactoryMap; 

	//! Returns the single instance of the factory registration
	static TeFactoryMap& instance()
	{ 
		static TeFactoryMap Fmap_;
		return Fmap_;
	}

protected:

	//!  Builds a new product from a set of parameters (should be implemented by descendants)
    virtual PRODUCT* build(PARAMS* /*arg*/)
	{ 
		return PRODUCT::DefaultObject(); 
	}

 	//!  Builds a new product without parameters (should be implemented by descendants)
   virtual PRODUCT* build()
	{ 
		return PRODUCT::DefaultObject(); 
	}

public:
	//! Factory Constructor
	/*
		\param prodId the identification of the product that the factory produces
	*/
	TeAbstractFactory(PRODUCTIDTYPE prodId);

	//! Destructor
	virtual ~TeAbstractFactory() {}

	//! Virtual constructor: make the product from some arguments
	/*
		\param arg a pointer to an structure that contains the arguments
		necessary to build the product. The type of product has to be extracted
		from the arguments
	*/
	static PRODUCT* make(PARAMS* arg);

	//! Virtual constructor: explictly identifies which product should be made
	static PRODUCT* make(PRODUCTIDTYPE prodId);

private:
	PRODUCTIDTYPE  productIdentification_;
};

// Factor Constructor
template <typename PRODUCT, typename PARAMS, typename PRODUCTIDTYPE>
TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::TeAbstractFactory(PRODUCTIDTYPE prodId):
	productIdentification_(prodId)
{
	TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::instance()[prodId] = this;
}

//! Builds an object, based on the input parameters
template <typename PRODUCT, typename PARAMS, typename PRODUCTIDTYPE> 
PRODUCT*
TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::make(PRODUCTIDTYPE producId)
{
	// try to find the name on the factory dictionary
	typename TeFactoryMap::iterator i = TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::instance().find(producId);

	// Not found ?  return the Default Object   
	if (i == TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::instance().end())
		return PRODUCT::DefaultObject();

	// Create an object, based on the input parameters
	return (*i).second->build();
}

//! Builds an object, based on the input parameters
template <typename PRODUCT, typename PARAMS, typename PRODUCTIDTYPE> 
PRODUCT*
TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::make(PARAMS* arg)
{
	// If there are no arguments or factory identification return the default object
	if (!arg)
		return PRODUCT::DefaultObject();

	PRODUCTIDTYPE productId = arg->getProductId(); 

	// try to find the name on the factory dictionary
	typename TeFactoryMap::iterator i = TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::instance().find(productId);

	// Not found ?  return the Default Object   
	if (i == TeAbstractFactory<PRODUCT,PARAMS,PRODUCTIDTYPE>::instance().end())
		return PRODUCT::DefaultObject();

	// Create an object, based on the input parameters
		return (*i).second->build(arg);
}
#endif

