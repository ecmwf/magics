/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Label.h
    \brief Implementation of the Template class Label.
    
    Magics Team - ECMWF 2004
    
    Started: Tue 16-Mar-2004
    
    Changes:
    
*/

#include "Label.h"

using namespace magics;

Label::Label(const string& label) : label_(label),
	justification_(MCENTRE), verticalAlign_(MBASE),  angle_(0.), 
	blanking_(false), visible_(false) 
{
}



Label::Label(double label) : label_(tostring(label)),  
		justification_(MCENTRE), verticalAlign_(MBASE), angle_(0.), 
		blanking_(false), visible_(false) 
{

}

Label::~Label() 
{}

/*!
 Class information are given to the output-stream.
*/		
void Label::print(ostream& out)  const
{
	out << "Label[";
	out << "label = " << label_;
	out << ",visible = " << visible_;
	out << ",blanking = " << blanking_;
	out << ",font = " << font_;
	out << ",angle = " << angle_;
	out << "]";
}
