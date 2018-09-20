/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionList.cc
    \brief Implementation of the Template class ColourTableDefinitionList.
    
    Magics Team - ECMWF 2005
    
    Started: Mon 4-Jul-2005
    
    Changes:
    
*/



#include "ColourTableDefinitionList.h"
#include "XmlNode.h"

using namespace magics;

ColourTableDefinitionList::ColourTableDefinitionList() 
{
}


ColourTableDefinitionList::~ColourTableDefinitionList() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void ColourTableDefinitionList::print(ostream& out)  const
{
	out << "ColourTableDefinitionList[";
	out << "]";
}

void ColourTableDefinitionList::set(const ColourTableDefinitionListInterface& attributes)
{
	colours_ = attributes.getValues();

}

void ColourTableDefinitionList::set(const XmlNode& node)
{	
	for (auto &elt : node.elements())
	{
		try {
			Colour* colour = MagTranslator<string, Colour>()(elt->name());
			colour->set(*elt);
			colours_.push_back(colour->name());
			MagLog::dev() << "add colour---" << colour->name() << "\n";
		}
		catch (NoFactoryException& e) {}
	}
}


void  ColourTableDefinitionList::set(ColourTable& table, int nb)
{
	prepare();
	if ( colours_.empty() ) {
		MagLog::warning() << "Colour Table Definition: No colour defined\n";
		colours_.push_back("blue");
		colours_.push_back("green");
		colours_.push_back("yellow");
		colours_.push_back("orange");
		colours_.push_back("red");
	}
	 
	stringarray::const_iterator colour = colours_.begin();
	// Nb is the number of intervals! 
	// We need nb-1 colours!
	for ( int i = 0; i < nb-1; i++) {
        
		if ( colour == colours_.end() ) {
				if (policy_ == M_LASTONE)
					table.push_back(Colour(colours_.back()));
				else {
					colour = colours_.begin();
					table.push_back(Colour(*colour));
					colour++;
				}

			
		}
		else {
            table.push_back(Colour(*colour));
		    colour++;
        }
	}
}
