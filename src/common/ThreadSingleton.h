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

// File ThreadSingleton.h
// Baudouin Raoult - ECMWF Jun 96

#ifndef ThreadSingleton_H
#define ThreadSingleton_H

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

template<class T> class ThreadSingleton {
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
	static pthread_key_t  key_;

// -- Class methods

	static void init(void);
	static void cleanUp(void*);

};


#include "ThreadSingleton.cc"

#endif
