/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file GradientsColourTechnique.cc
    \brief Implementation of the Template class GradientsColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#include "GradientsColourTechnique.h"
#include "LevelSelection.h"
#include "ColourTableDefinitionCompute.h"

using namespace magics;

GradientsColourTechnique::GradientsColourTechnique() 
{
}


GradientsColourTechnique::~GradientsColourTechnique() 
{
}

void GradientsColourTechnique::set(ColourTable& table, int nb) const
{
	
}

/*!
 Class information are given to the output-stream.
*/		
void GradientsColourTechnique::print(ostream& out)  const
{
	out << "GradientsColourTechnique[";
	out << "]";
}

void GradientsColourTechnique::set(const ColourTechniqueInterface& attributes)
{
   
}

