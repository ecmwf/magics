/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TitleStringField.cc
    \brief Implementation of the Template class TitleStringField.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/

#include "TitleStringField.h"

using namespace magics;

TitleStringField::TitleStringField(const string& text) : text_(text) 
{
}


TitleStringField::~TitleStringField() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void TitleStringField::print(ostream& out)  const
{
	out << text_; 
}


void TitleStringField::operator()(vector<string>& lines) const
{
	lines.back() += text_;
}