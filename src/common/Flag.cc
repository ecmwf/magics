/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Flag.cc
    \brief Implementation of the Template class Flag.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 16-Mar-2005
    
    Changes:
*/

#include "Flag.h"

using namespace magics;


Flag::Flag() : length_(1), convention_(SI)
{
}


Flag::~Flag() 
{
}

/*!
 Class information are given to the output-stream.
*/	
	
void Flag::print(ostream& out)  const
{
	out << "Flag";
}
	
