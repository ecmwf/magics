/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TitleMetaField.cc
    \brief Implementation of the Template class TitleMetaField.
    
    Magics Team - ECMWF 2004
    
    Started: Mon 21-Jun-2004
    
    Changes:
    
*/



#include "TitleMetaField.h"

using namespace magics;

TitleMetaField::TitleMetaField(const string& token) : token_(token) 
{
}


TitleMetaField::~TitleMetaField() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void TitleMetaField::print(ostream& out)  const
{
    if ( token_ == "newline") {
        //out << ends;
        return;
    }
	out << "TitleMetaField[";
    out << "token=" << token_;
    for (const_iterator attribute = begin(); attribute != end(); ++attribute) 
        out << ", " <<  attribute->first << "=" << attribute->second;
	out << "]";
}

void TitleMetaField::operator()(vector<string>& title ) const
{
	
	 ostringstream out;
		out << "TitleMetaField[";
	    out << "token=" << token_;
	    for (const_iterator attribute = begin(); attribute != end(); ++attribute) 
	        out << ", " <<  attribute->first << "=" << attribute->second;
		out << "]";
	    
	    title.back() += out.str();
}

