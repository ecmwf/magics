/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file TableDefinitionList.cc
    \brief Implementation of the Template class TableDefinitionList.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/

#include "TableDefinitionList.h"
#include "XmlNode.h"

using namespace magics;


template <class T>
void TableDefinitionList<T>::set(const TableDefinitionListInterface<T>& attributes) {
    const vector<T>& values = attributes.getValues();
    std::copy(values.begin(), values.end(), this->begin());
}

template <class T>
inline void fromString(const string& str, T& out) {
    std::stringstream ss(str);
    ss >> out;
}

template <class T>
void TableDefinitionList<T>::set(const XmlNode& node) {
    MagLog::info() << "TableDefinitionList::set(const XmlNode&): to be implemented\n";
    MagLog::dev() << "Node to be interpreted ---> " << node << endl;

    for (auto& elt : node.elements()) {
        if (magCompare(elt->name(), "value")) {
            // convert the value ..
            double val;
            fromString(elt->data(), val);

            this->push_back(val);
        }
    }
}
