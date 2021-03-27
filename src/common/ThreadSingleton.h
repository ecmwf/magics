/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// File ThreadSingleton.h
// Baudouin Raoult - ECMWF Jun 96

#ifndef ThreadSingleton_H
#define ThreadSingleton_H

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

template <class T>
class ThreadSingleton {
public:
    // -- Contructors

    ThreadSingleton();

    // -- Destructor

    ~ThreadSingleton();

    // -- Class methods

    static T& instance();

private:
    // No copy allowed

    ThreadSingleton(const ThreadSingleton<T>&);
    ThreadSingleton<T>& operator=(const ThreadSingleton<T>&);

    // -- Class members

    static pthread_once_t once_;
    static pthread_key_t key_;

    // -- Class methods

    static void init(void);
    static void cleanUp(void*);
};


#include "ThreadSingleton.cc"

#endif
