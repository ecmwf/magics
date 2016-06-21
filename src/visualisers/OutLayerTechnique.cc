/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HeightSelection.cc
    \brief Implementation of the Template class HeightSelection.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/



#include "OutLayerTechnique.h"

using namespace magics;

NoOutLayerTechnique::NoOutLayerTechnique() 
{
}


NoOutLayerTechnique::~NoOutLayerTechnique() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void NoOutLayerTechnique::print(ostream& out)  const
{
	out << "OutLayerTechnique[";
	out << "]";
}

SimpleOutLayerTechnique::SimpleOutLayerTechnique() 
{
}


SimpleOutLayerTechnique::~SimpleOutLayerTechnique() 
{
}

