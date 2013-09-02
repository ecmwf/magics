/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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

void CalculateColourTechnique::set(ColourTable& table, int nb) const
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

