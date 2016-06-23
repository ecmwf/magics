/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file NetcdfMarsConvention.cc
    \brief Implementation of the Template class NetcdfMarsConvention.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 17-Feb-2004
    
    Changes:
    
*/



#include "NetcdfMarsConvention.h"
#include "Factory.h"

using namespace magics;

NetcdfMarsConvention::NetcdfMarsConvention() 
{
}


NetcdfMarsConvention::~NetcdfMarsConvention() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void NetcdfMarsConvention::print(ostream& out)  const
{
	out << "NetcdfMarsConvention[";
	out << "]";
}

static SimpleObjectMaker<NetcdfMarsConvention, NetcdfConvention>  netcdf_mars_convention("mars");
