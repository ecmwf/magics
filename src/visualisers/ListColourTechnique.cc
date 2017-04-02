/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ListColourTechnique.cc
    \brief Implementation of the Template class ListColourTechnique.
    
    Magics Team - ECMWF 2004
    
    Started: Wed 18-Aug-2004
    
    Changes:
    
*/


#include "ListColourTechnique.h"
#include "LevelSelection.h"
#include "ColourTableDefinitionList.h"

using namespace magics;

ListColourTechnique::ListColourTechnique() 
{
    
}

void ListColourTechnique::set(LevelSelection&, LevelSelection&, ColourTable& table, int nb) const
{
    ColourTableDefinitionList helper;
    //policy = table.getPolicy();
    helper.set(*this);
    helper.set(table, nb);
}


ListColourTechnique::~ListColourTechnique() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ListColourTechnique::print(ostream& out)  const
{
	out << "ListColourTechnique[";
	out << "]";
}

void ListColourTechnique::set(const ColourTechniqueInterface& attributes)
{
	values_ = attributes.getColours();
	policy_ = attributes.getPolicy();
}
