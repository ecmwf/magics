/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file CalculateColourTechnique.cc
    \brief Implementation of the Template class CalculateColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/

#include "CalculateColourTechnique.h"
#include "LevelSelection.h"
#include "ColourTableDefinitionCompute.h"

using namespace magics;

CalculateColourTechnique::CalculateColourTechnique() 
{
}

CalculateColourTechnique::~CalculateColourTechnique() 
{
}

void CalculateColourTechnique::set(LevelSelection&, LevelSelection&, ColourTable& table, int nb) const
{
	ColourTableDefinitionCompute helper;
	helper.set(*this);
	helper.set(table, nb);
}

/*!
 Class information are given to the output-stream.
*/		
void CalculateColourTechnique::print(ostream& out)  const
{
	out << "CalculateColourTechnique[";
	out << "]";
}

void CalculateColourTechnique::set(const ColourTechniqueInterface& attributes)
{
    max_ = auto_ptr<Colour>(attributes.getMaxColour().clone());
    min_ = auto_ptr<Colour>(attributes.getMinColour().clone());
	direction_ = attributes.getDirection();
}

