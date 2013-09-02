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

 ******************************** LICENSE ********************************//******************************** LICENSE ********************************

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

// File MutexCond.h
// Baudouin Raoult - ECMWF Jun 96

#ifndef MutexCond_H
#define MutexCond_H

#ifndef Mutex_H
#include "Mutex.h"
#endif

// A mutex and a condition variable
// for Producer/Consumer architectures

class MutexCond {
public:

// -- Contructors

	MutexCond(char tag = ' ');

// -- Destructor

	~MutexCond();

// -- Methods

	void lock();
	void unlock();
	void wait();
	void signal();
	void broadcast();
	bool wait(int);
	char tag() const { return tag_; }

private:

// No copy allowed

	MutexCond(const MutexCond&);
	MutexCond& operator=(const MutexCond&);

// -- Members

	pthread_mutex_t mutex_;
	pthread_cond_t  cond_;
	char tag_;
	bool inited_;

};

#endif
