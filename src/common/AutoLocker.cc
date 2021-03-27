/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/* #define CHECK_DEAD_LOCKS */

#ifndef AutoLock_H
#include "AutoLock.h"
#endif

#ifndef Mutex_H
#include "Mutex.h"
#endif

#ifndef MagExceptions_H
#include "MagExceptions.h"
#endif

typedef map<void*, pthread_t, std::less<void*> > GotMap;
typedef map<pthread_t, void*, std::less<pthread_t> > WantMap;
static WantMap* wantMap = 0;
static GotMap* gotMap   = 0;

typedef set<pthread_t, std::less<pthread_t> > Set;


void AutoLocker::want(void* resource) {
#ifdef CHECK_DEAD_LOCKS
    pthread_once(&once, init);
    mutex->lock();

    GotMap::iterator i = gotMap->find(resource);

    if (i != gotMap->end()) {
        if ((*i).second != pthread_self()) {
            (*wantMap)[pthread_self()] = resource;
            analyse(resource);
        }
    }

    mutex->unlock();
#endif
}

void AutoLocker::got(void* resource) {
#ifdef CHECK_DEAD_LOCKS
    mutex->lock();
    (*gotMap)[resource] = pthread_self();
    wantMap->erase(pthread_self());
    mutex->unlock();
#endif
}

void AutoLocker::release(void* resource) {
#ifdef CHECK_DEAD_LOCKS
    mutex->lock();
    gotMap->erase(resource);
    mutex->unlock();
#endif
}

static void visit(pthread_t p, Set& s, void* resource) {
    if (s.find(p) != s.end())
        Panic("Deadlock detected");

    s.insert(p);

    GotMap::iterator i = gotMap->find(resource);
    pthread_t q        = (*i).second;  // The one with the resource

    WantMap::iterator j = wantMap->find(q);
    if (j != wantMap->end())
        visit(q, s, (*j).second);

    s.erase(p);
}

void AutoLocker::analyse(void* resource) {
    Set set;
    visit(pthread_self(), set, resource);
}
