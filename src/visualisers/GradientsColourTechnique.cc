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

void GradientsColourTechnique::set(LevelSelection& out, LevelSelection& in, ColourTable& table, int nb) const
{
	// First make sure that 
	ColourTableDefinitionCompute helper;


	vector<string>::const_iterator col = colours_.begin();
	vector<double>::const_iterator val = values_.begin();
	vector<int>::const_iterator step = steps_.begin();
	vector<string>::const_iterator technique = techniques_.begin();
	
	if ( colours_.empty() ) {
		MagLog::warning() << " No colours given to the gradients method" << endl;
		return;
	}
	if ( values_.empty() ) {
		MagLog::warning() << " No intervals given to the gradients method" << endl;
		return;
	}
	string left = *col;
	++col;
	double from = *val;
	++val;
	out.clear();
	in.clear();
	
	while ( col != colours_.end() ) {
		string right = *col;
		double to = *val;

		int istep = ( steps_.empty() || steps_.empty() ) ? 10 : *step;
		string stechnique = ( techniques_.empty() || technique == techniques_.end() ) ? "linear" : *technique;
		ColourTableDefinitionCompute helper(left, right, stechnique);
		helper.set(table, (istep)+1);
		double increment = ((to-from)/(istep) );
		for (int i = 0; i < istep; i++) {
			in.push_back(from + (i*increment));
			out.push_back(from + (i*increment));
		}
		left = right;
		from = to;
		++col;
		
		++val;
		
		if ( step != steps_.end() )
			++step;
		if ( technique == techniques_.end() )
			++technique;

	}
	// add the last entry ! 
	in.push_back(from);
	out.push_back(from);

	
	// now we compute the new levels list :



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

