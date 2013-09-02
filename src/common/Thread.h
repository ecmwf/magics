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

// File Thread.h
// Baudouin Raoult - ECMWF Jun 96

#ifndef Thread_H
#define Thread_H

#ifndef Mutex_H
#include "Mutex.h"
#endif


#include "ThreadControler.h"

// This should be a Task

class Thread {
public:
	friend class magics::ThreadControler;

// -- Contructors
	
	Thread(bool autodel = true); // 

// -- Destructor

	virtual ~Thread();

// -- Methods

	void stop();

protected:

// -- Members

	Mutex mutex_;

// -- Methods

	bool  stopped();

private:

// No copy allowed

	Thread(const Thread&);
	Thread& operator=(const Thread&);

// -- Members

	bool    stop_;
	bool    autodel_;
#ifdef linux
	void*   data_;
#endif

// -- Methods
	
	virtual void run() = 0;

};

#endif
