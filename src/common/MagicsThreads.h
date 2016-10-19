/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
