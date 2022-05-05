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
#include "ColourTableDefinitionCompute.h"
#include "LevelSelection.h"

using namespace magics;

ListColourTechnique::ListColourTechnique() {}

void ListColourTechnique::set(LevelSelection&, LevelSelection&, ColourTable& table, int nb) {
    ColourTableDefinitionCompute helper;
    if (colours_.empty()) {
        MagLog::warning() << "Colour Table Definition: No colour defined\n";
    
        vector<string> colours = { "blue", "green","yellow", "orange", "red" };
        helper.set(colours, table, nb, list_policy_);

        return;
    }
    if ( reverse_ )
        std::reverse(colours_.begin(),colours_.end());


    
    helper.set(colours_, table, nb, list_policy_);
}


ListColourTechnique::~ListColourTechnique() {}

/*!
 Class information are given to the output-stream.
*/
void ListColourTechnique::print(ostream& out) const {
    out << "ListColourTechnique[";
    out << "]";
}

void ListColourTechnique::set(const ColourTechniqueInterface& attributes) {
    colours_ = attributes.getColours();
}
