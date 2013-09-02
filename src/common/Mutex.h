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

// File Mutex.h
// Baudouin Raoult - ECMWF May 96

#ifndef Mutex_H
#define Mutex_H

#ifdef machine_H
#define marsmachine_H
#endif

#ifndef marsmachine_H
#include "marsmachine.h"
#endif

class Mutex {
public:

// -- Contructors

	Mutex(char tag = ' ');

// -- Destructor

	~Mutex();

// -- Methods

	void lock();
	void unlock();
	char tag() const { return tag_; }

// -- Class methods

	static Mutex& global();

private:

// No copy allowed

	Mutex(const Mutex&);
	Mutex& operator=(const Mutex&);

protected:

// -- Members

	pthread_mutex_t mutex_;
	bool            exists_;
	char            tag_;

};

#endif
