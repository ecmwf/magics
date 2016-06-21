/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SelectionMode.cc
    \brief Implementation of the Template class SelectionMode.
    
    Started: Tue 20-Nov-2007    
*/



#include "SelectionMode.h"

using namespace magics;

SelectionMode::SelectionMode() 
{
}


SelectionMode::~SelectionMode() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void SelectionMode::print(ostream& out)  const
{
	out << "SelectionMode[";
	out << "]";
}

