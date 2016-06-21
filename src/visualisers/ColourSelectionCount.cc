/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourSelectionCount.cc
    \brief Implementation of the Template class ColourSelectionCount.
    
    Magics Team - ECMWF 2004
    
    Started: Thu 20-May-2004
    
    Changes:
    
*/



#include "ColourSelectionCount.h"

using namespace magics;

ColourSelectionCount::ColourSelectionCount() 
{
}


ColourSelectionCount::~ColourSelectionCount() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ColourSelectionCount::print(ostream& out)  const
{
	out << "ColourSelectionCount[";
	out << "]";
}


void ColourSelectionCount::prepare() 
{
	if ( !empty() ) return;
	calculate(min_, max_, count_);
}

static SimpleObjectMaker<ColourSelectionCount, ColourSelection> colour_selection_count("count");
