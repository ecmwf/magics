/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

#ifndef Mutex_H
#include "Mutex.h"
#endif

#ifndef MagExceptions_H
#include "MagExceptions.h"
#endif

#include <errno.h>


#if 0
static int _tc(int a, const char* m)
{

	return a;
}

#undef THRCALL
#define THRCALL(a) _tc(a, #a)
#endif


Mutex::Mutex(char tag) : exists_(false), tag_(tag) {
#if defined(__GNUC__) && __GNUC__ < 3
#ifndef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#define PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP \
    { PTHREAD_MUTEX_RECURSIVE_NP }
#endif
    pthread_mutexattr_t attr = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#else
    pthread_mutexattr_t attr;
    THRCALL(::pthread_mutexattr_init(&attr));
    THRCALL(::pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE));
#endif

    THRCALL(pthread_mutex_init(&mutex_, &attr));

    exists_ = true;
    THRCALL(::pthread_mutexattr_destroy(&attr));
}

Mutex::~Mutex() {
    THRCALL(pthread_mutex_destroy(&mutex_));
}

void Mutex::lock(void) {
    if (!exists_) {
        cerr << "Mutex used before being contructed" << endl;
        return;
    }

    THRCALL(pthread_mutex_lock(&mutex_));
}

void Mutex::unlock(void) {
    if (!exists_) {
        cerr << "Mutex used before being contructed" << endl;
        return;
    }
    THRCALL(pthread_mutex_unlock(&mutex_));
}

//=============================================================

static Mutex globalMutex;

Mutex& Mutex::global() {
    return globalMutex;
}
