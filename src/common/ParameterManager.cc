/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ParameterManager.cc
Magics Team - ECMWF 2004

*/

#include "ParameterManager.h"

using namespace magics;

ParameterManager* ParameterManager::table_ = 0;

ParameterManager::ParameterManager() {}


ParameterManager::~ParameterManager() {}


void ParameterManager::print(ostream& out) const {
    out << "ParameterManager";
    string sep = "[";
    for (const_iterator entry = begin(); entry != end(); ++entry) {
        out << sep << (*(*entry).second);
        sep = ",";
    }
    out << "]";
}

BaseParameter* ParameterManager::parameter(const string& name) const {
    string lower  = lowerCase(name);
    size_type pos = lower.find_first_of(" ");

    string tofind        = (pos != string::npos) ? lower.substr(0, pos) : lower;
    const_iterator param = find(tofind);
    if (param != end()) {
        return (*param).second;
    }
    MagLog::info() << "The parameter " << name << " is unknown in Magics++.\n"
                   << "Please check the documentation or contact\n"
                   << "the Meteorological Visualisation Section at ECMWF.\n";
    return 0;
}

void ParameterManager::resetAll() {
    for (iterator entry = begin(); entry != end(); ++entry) {
        entry->second->reset();
    }
}

void ParameterManager::reset() {
    if (table_)
        table_->resetAll();
}

void ParameterManager::add(const string& name, BaseParameter* param) {
    if (!table_)
        table_ = new ParameterManager();
    (*table_)[name] = param;
}


void ParameterManager::set(const string& name, const char* value) {
    ASSERT(value);
    set(name, std::string(value));
}

void ParameterManager::reset(const string& name) {
    ASSERT(table_);
    BaseParameter* param = (*table_).parameter(name);
    if (param)
        param->reset();
}

void ParameterManager::release() {
    delete table_;
    table_ = nullptr;
}

BaseParameter* ParameterManager::getCopy(const string& name) {
    ASSERT(table_);
    BaseParameter* param = (*table_).parameter(name);
    ASSERT(param);
    return (param) ? param->clone() : 0;
}

double ParameterManager::getDouble(const string& name) {
    double value;
    ASSERT(get(name, value));
    return value;
}

int ParameterManager::getInt(const string& name) {
    int value;
    ASSERT(get(name, value));
    return value;
}
unsigned long long ParameterManager::getULong(const string& name) {
    unsigned long long value;
    
    ASSERT(get(name, value));
    return value;
}

string ParameterManager::getString(const string& name) {
    string value;
    ASSERT(get(name, value));
    return value;
}

stringarray ParameterManager::getStringArray(const string& name) {
    stringarray value;
    ASSERT(get(name, value));
    return value;
}

doublearray ParameterManager::getDoubleArray(const string& name) {
    doublearray value;
    ASSERT(get(name, value));
    return value;
}

intarray ParameterManager::getIntArray(const string& name) {
    intarray value;
    ASSERT(get(name, value));
    return value;
}

longintarray ParameterManager::getLongIntArray(const string& name) {
    longintarray value;
    ASSERT(get(name, value));
    return value;
}

bool ParameterManager::getBool(const string& name) {
    string s;
    ASSERT(get(name, s));
    s = lowerCase(s);

    if (s == "no" || s == "off" || s == "false")
        return false;
    if (s == "yes" || s == "on" || s == "true")
        return true;

    // Catter for ints
    return atoi(s.c_str());
}
