/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

/* #define CHECK_DEAD_LOCKS */

#ifndef   AutoLock_H
#include "AutoLock.h"
#endif

#ifndef   Mutex_H
#include "Mutex.h"
#endif

#ifndef   MagExceptions_H
#include "MagExceptions.h"
#endif

typedef map<void*,pthread_t,std::less<void*> > GotMap;
typedef map<pthread_t,void*,std::less<pthread_t> > WantMap;
static WantMap*   wantMap = 0;
static GotMap*   gotMap = 0;
static Mutex* texmu = 0;
typedef set<pthread_t,std::less<pthread_t> > Set;

static pthread_once_t once = PTHREAD_ONCE_INIT;

static void lock()
{
#ifdef CHECK_DEAD_LOCKS
	 mutex->lock();
#endif
}

static void unlock()
{
#ifdef CHECK_DEAD_LOCKS
	mutex->unlock();
#endif
}

static void init(void)
{
    texmu   = new Mutex;
	wantMap = new WantMap;
	gotMap  = new GotMap;
	pthread_atfork(lock,unlock,unlock);
}


void AutoLocker::want(void* resource)
{
#ifdef CHECK_DEAD_LOCKS
	pthread_once(&once,init);
	mutex->lock();

	GotMap::iterator i = gotMap->find(resource);

	if( i != gotMap->end())
	{
		if((*i).second != pthread_self())
		{
			(*wantMap)[pthread_self()] = resource;
			analyse(resource);
		}
	}

	mutex->unlock();
#endif
}

void AutoLocker::got(void* resource)
{
#ifdef CHECK_DEAD_LOCKS
	mutex->lock();
	(*gotMap)[resource] = pthread_self();
	wantMap->erase(pthread_self());
	mutex->unlock();
#endif
}

void AutoLocker::release(void* resource)
{
#ifdef CHECK_DEAD_LOCKS
	mutex->lock();
	gotMap->erase(resource);
	mutex->unlock();
#endif
}

static void visit(pthread_t p, Set& s,void *resource)
{
	if(s.find(p) != s.end())
		Panic("Deadlock detected");
	
	s.insert(p);
	
	GotMap::iterator i = gotMap->find(resource);
	pthread_t q = (*i).second; // The one with the resource

	WantMap::iterator j = wantMap->find(q);
	if(j != wantMap->end())
		visit(q,s,(*j).second);

	s.erase(p);
}

void AutoLocker::analyse(void *resource)
{
	Set set;
	visit(pthread_self(),set,resource);
}
