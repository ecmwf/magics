/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file HeightTableDefinition.h
    \brief Definition of Colour class.

    Magics Team - ECMWF 2004

    Started by Sylvie Lamy-Thepaut ECMWF 2002

    Changes:



*/
#ifndef HeightTableDefinition_H
#define HeightTableDefinition_H

#include "TableDefinition.h"
#include "TableDefinitionCompute.h"
#include "TableDefinitionList.h"

#include "Factory.h"
#include "MagTranslator.h"


namespace magics {


class HeightTableDefinition : public TableDefinitionInterface<double> {
public:
    HeightTableDefinition() {}
    HeightTableDefinition* clone() const { return new HeightTableDefinition(); }
    virtual void toxml(ostream&, int) const {}
};

class HeightTableDefinitionList : public HeightTableDefinition {
public:
    HeightTableDefinitionList() { helper_ = new TableDefinitionList<double>(); }
    HeightTableDefinition* clone() const { return new HeightTableDefinitionList(); }
};

class HeightTableDefinitionCompute : public HeightTableDefinition {
public:
    HeightTableDefinitionCompute() { helper_ = new TableDefinitionCompute<double>(); }
    HeightTableDefinition* clone() const { return new HeightTableDefinitionCompute(); }
};


template <>
class MagTranslator<string, HeightTableDefinition> {
public:
    HeightTableDefinition* operator()(const string& val) {
        return SimpleObjectMaker<HeightTableDefinition>::create(val);
    }
    HeightTableDefinition* magics(const string& param) {
        HeightTableDefinition* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics

#endif
