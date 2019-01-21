/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file LevelTableDefinition.h
    \brief Definition of Colour class.

    Magics Team - ECMWF 2004

    Started by Sylvie Lamy-Thepaut ECMWF 2002

    Changes:



*/
#ifndef LevelTableDefinition_H
#define LevelTableDefinition_H

#include "TableDefinition.h"
#include "TableDefinitionCompute.h"
#include "TableDefinitionList.h"

#include "Factory.h"
#include "MagTranslator.h"


namespace magics {

class LevelTableDefinition : public TableDefinitionInterface<double> {
public:
    LevelTableDefinition() {}
    LevelTableDefinition* clone() const { return new LevelTableDefinition(); }
    virtual void toxml(ostream&, int) const {}
};

class LevelTableDefinitionList : public LevelTableDefinition {
public:
    LevelTableDefinitionList() { helper_ = new TableDefinitionList<double>(); }
    LevelTableDefinition* clone() const { return new LevelTableDefinitionList(); }
};

class LevelTableDefinitionCompute : public LevelTableDefinition {
public:
    LevelTableDefinitionCompute() { helper_ = new TableDefinitionCompute<double>(); }
    LevelTableDefinition* clone() const { return new LevelTableDefinitionCompute(); }
};


template <>
class MagTranslator<string, LevelTableDefinition> {
public:
    LevelTableDefinition* operator()(const string& val) { return SimpleObjectMaker<LevelTableDefinition>::create(val); }
    LevelTableDefinition* magics(const string& param) {
        LevelTableDefinition* object;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics

#endif
