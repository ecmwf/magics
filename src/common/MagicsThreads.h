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

/*! \file MagicsThreads.h
    \brief Definition of the Template class MagicsThreads.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 7-May-2004
    
    Changes:
    
*/

#ifndef MagicsThreads_H
#define MagicsThreads_H

#include "magics.h"
#include <pthread.h>
#include <stack>
using std::stack;

namespace magics {

class MagicsThreads;
class MagicsTask 
{
public :
	MagicsTask(MagicsThreads& manager) : manager_(manager){}
	virtual ~MagicsTask() {}
	virtual void run() = 0;   
protected :
	MagicsThreads& manager_; 
};

class AutoLock
{
public:
	AutoLock(pthread_mutex_t& mutex) :mutex_(mutex) 
		{  pthread_mutex_lock(&mutex_); }
  
	~AutoLock() 
		{  pthread_mutex_unlock(&mutex_); }
private:
	pthread_mutex_t& mutex_;
};

class MagicsThreads : public stack<MagicsTask*> {

public:
	MagicsThreads();
	virtual ~MagicsThreads();
    
	void queue(MagicsTask*);
	void signal();
	void loop();

protected:
	//! Method to print string about this class on to a stream of type ostream (virtual).
	virtual void print(ostream&) const; 
	pthread_mutex_t mutex_;
	pthread_cond_t condition_;
	bool ok_;

private:
	//! Copy constructor - No copy allowed
	MagicsThreads(const MagicsThreads&);
	//! Overloaded << operator to copy - No copy allowed
	MagicsThreads& operator=(const MagicsThreads&);

// -- Friends
	//! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const MagicsThreads& p)
		{ p.print(s); return s; }
};

} // namespace magics
#endif
