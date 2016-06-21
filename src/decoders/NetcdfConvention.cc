/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfConvention.cc
    \brief Implementation of the Template class NetcdfConvention.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/

#include "NetcdfConvention.h"

using namespace magics;

NetcdfConvention::NetcdfConvention() 
{
}


NetcdfConvention::~NetcdfConvention() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void NetcdfConvention::print(ostream& out)  const
{
	out << "NetcdfConvention[";
	out << "]";
}

