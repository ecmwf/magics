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

	
	
	
	if ( colours_.size() < 2 ) {
		MagLog::warning() << " No enough colours given to the gradients method" << endl;
		return;
	}
	vector<double> stops = in;
	if ( stops.empty() ) {
		MagLog::warning() << " No intervals given to the gradients method, guessing ..." << endl;
		double min = in.front();
		double max = in.back();
		double increment = (max-min)/(colours_.size() -1);
		for ( double i = 0; i < colours_.size(); i++)
			stops.push_back(min + (i*increment));
	}

	vector<double>::const_iterator val = stops.begin();
	vector<int>::const_iterator step = steps_.begin();
	string stop_method = lowerCase(stop_method_);
	
	out.clear();
	in.clear();
	
  	//ColourTable colours;
  	int last = colours_.size() -1;
  	

  	for ( int col = 1; col < colours_.size(); ++col) {
  		string left = colours_[col-1];
  		string right = colours_[col];	
  		int istep = ( steps_.empty() ) ? 10 : *step;
  		
  		// right
  		ColourTableDefinitionCompute helper(left, right, technique_, technique_direction_);
  		
  		int nb;
  		
  		if ( stop_method == "right") 
  			nb = (col == 1 ) ? istep + 1 : istep + 2;
  		else if ( stop_method == "left" )
  			nb = (col == last ) ? istep + 1 : istep + 2;
  		else if ( stop_method == "ignore" ) 
  			nb = (col == 1 || col == last ) ? istep + 2 : istep + 3;
  		else
  			nb = istep+1;

  		ColourTable workingtable;
  		helper.set(workingtable, nb);

  		int all = workingtable.size();
  		
  		int use;
	  	if ( stop_method == "right") 
	  		use = (col == last ) ? all : all-1;  		
	  	else if ( stop_method == "left") 
	  		use = (col == 1 ) ? all : all-1;
	  	else if ( stop_method == "ignore") 
	  		use = (col == 1 || col == last ) ? all-1 : all-2;
	  	else 
	  		use = all;

	  	for (int c = 0; c < use; ++c ) 
	  		table.push_back(workingtable[c]);
	  	
		// Next block
		if ( !steps_.empty()) { 
			++step;
			if ( step == steps_.end() )
				--step;
		}

  	}
	
	
	
	step = steps_.begin();
	int col = 0;

	// Now the interval ...
  	for (int stop = 1; stop < stops.size(); ++stop) { 
  		  double from = stops[stop-1];
  		  double to = stops[stop];
  		  int istep = ( steps_.empty() ) ? 10 : *step;
		 
			
		  in.push_back(from);
		  out.push_back(from);
		  
		  double inc = (to - from )/(istep+1);
		  for (int i = 1; i < istep; i++) {
		  			in.push_back(from +(i*inc));
		  			out.push_back(from +(i*inc));
		  }
		    
		  
			if ( !steps_.empty()) { 
			++step;
			if ( step == steps_.end() )
				--step;
		}
	}

	
	



	
	



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

