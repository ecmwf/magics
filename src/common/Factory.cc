/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Factory.cc
    \brief Definition of Parameter base class.

    Magics Team - ECMWF 2004

    Started: Jan 2004

    Changes:

*/

#include "MagLog.h"
using namespace magics;
using namespace std;  // to run on AIX

template <class B>
map<string, SimpleFactory<B>*>* SimpleFactory<B>::map_ = 0;


template <class B>
SimpleFactory<B>::SimpleFactory(const string& name) : name_(lowerCase(name)) {
    if (!map_) {
        map_ = new map<string, SimpleFactory<B>*>();
    }
    // This happens in the automatically gerenated files
    // if(map_->find(name_) != map_->end()) {
    //     std::cerr << "SimpleFactory: duplicate factory name: " + name_ << std::endl;
    // }
    (*map_)[name_] = this;
}


template <class B>
SimpleFactory<B>::~SimpleFactory() {
    ASSERT(map_);
    map_->erase(name_);
}

template <class B>
B* SimpleFactory<B>::create(const string& name) {
    SimpleFactory<B>* maker = get(name);
    ASSERT(maker);

    B* object = (*maker).make();
    return object;
}


template <class B>
SimpleFactory<B>* SimpleFactory<B>::get(const string& name) {
    ASSERT(map_);
    typename map<string, SimpleFactory<B>*>::iterator maker = (*map_).find(lowerCase(name));
    if (maker != (*map_).end()) {
        return (*maker).second;
    }

    MagLog::error() << "No factory named [" << name << "], values:" << std::endl;
    for (auto k = map_->begin(); k != map_->end(); ++k) {
        MagLog::error() << "  " << (*k).first << std::endl;
    }

    throw NoFactoryException(name);
}
