
/******************************  LICENSE  *******************************

 * (C) Copyright 1996-2017 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \\file Proj4EfasAttributes.h
    \\brief Definition of Proj4Efas Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 2020-09-24
*/

#include "MagRequest.h" 
#include "Proj4EfasWrapper.h"
#include "MagicsParameter.h"
#include "Factory.h"
#include "MagTranslator.h"

using namespace magics;


Proj4EfasWrapper::Proj4EfasWrapper(): proj4efas_(new Proj4Efas())

{

	
	
	Proj4ProjectionWrapper::object(proj4efas_);
	
	
} 
Proj4EfasWrapper::Proj4EfasWrapper(Proj4Efas* proj4efas): proj4efas_(proj4efas)
{
	
	
	Proj4ProjectionWrapper::object(proj4efas_);
	
} 

Proj4EfasWrapper::~Proj4EfasWrapper()
{
	
}

void Proj4EfasWrapper::set(const MagRequest& request)
{
	
	
	
	Proj4ProjectionWrapper::set(request);
	

	
	
}

void Proj4EfasWrapper::print(ostream& out)  const
{
	out << "Proj4EfasWrapper[]";
}


    


