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

/*! \file MagicsThreads.cc
    \brief Implementation of the Template class MagicsThreads.
    
    Magics Team - ECMWF 2004
    
    Started: Fri 7-May-2004
    
    Changes:
    
*/



#include "MagicsThreads.h"
#include "MagLog.h"
#include "MagException.h"

using namespace magics;


static void* do_it(void* data)
{
   MagLog::dev() << "do_it: ok???" << "\n";
   MagicsThreads* magics = (MagicsThreads*) (data);
   (*magics).loop();
  
   return 0;
}

static void* run_it(void* data)
{
   MagLog::dev() << "do_it: ok???" << "\n";
   MagicsTask* task = (MagicsTask*) (data);
   (*task).run();
  
   return 0;
}
MagicsThreads::MagicsThreads() : ok_(true)
{
    try {
        // create a Mutex to protect the stack!
         pthread_mutex_init(&mutex_, 0);
         pthread_cond_init(&condition_, 0);
    
        // create a condition to inform the loop thread that there is something to do!    
       //  create a thread ... 
       //  to start the loop method!
       
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
       
        pthread_t      thread;
        MagLog::dev() << "Thread ok???" << "\n";
        
        pthread_create(&thread,&attr,do_it,this);
        MagLog::dev() << "Thread ok!!!" << "\n";
        
	}
	catch (MagException& e) 
	{
        MagLog::error() << e.what() << "\n";
		
	}

   
   
}  


MagicsThreads::~MagicsThreads() 
{
}

/*!
 Class information are given to the output-stream.
*/		
void MagicsThreads::print(ostream& out)  const
{
	out << "MagicsThreads[";
	out << "]";
}

void MagicsThreads::queue(MagicsTask* task) 
{
    // Add a task to the stack ... 
    // Signal that a task has been added 
    
    {
        AutoLock lock(mutex_);
        push(task);
        MagLog::dev() << "send signal...." << "\n";
        pthread_cond_signal(&condition_);
    }
    // Signal...  
}

void MagicsThreads::signal()
{
    AutoLock lock(mutex_);
    ok_ = true;
    MagLog::dev() << "send signal...." << "\n";
    pthread_cond_signal(&condition_);
}

void MagicsThreads::loop() 
{
    while (true)  {           
            MagLog::debug() << "loop  OK... Wait..." << "\n";
            AutoLock look(mutex_);
            pthread_cond_wait(&condition_, &mutex_);
            MagLog::debug() << "loop  OK... Wake up..." << "\n";
            ok_ = false;
            if (!empty()) { 
              
                MagLog::debug() << " OK... Do IT!..." << "\n";
                
                // start a new thread an execute the task...
                pthread_attr_t attr;
                pthread_attr_init(&attr);
                pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
               
                pthread_t      thread;
                MagLog::dev() << "Thread ok???" << "\n";
                 MagicsTask* task = top();
                 pop();
                pthread_create(&thread,&attr,run_it, task);
                MagLog::dev() << "Thread ok!!!" << "\n";
            }
  
    }
        
}
