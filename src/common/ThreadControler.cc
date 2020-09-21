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

#include <assert.h>
#include <signal.h>

#if !(defined linux || defined magics_windows)
#include <sys/sched.h>
#endif

#ifndef AutoLock_H
#include "AutoLock.h"
#endif

#ifndef MagLog_H
#include "MagLog.h"
#endif

#ifndef MagExceptions_H
#include "MagExceptions.h"
#endif

#ifndef ThreadControler_H
#include "ThreadControler.h"
#endif

#ifndef Thread_H
#include "Thread.h"
#endif


using namespace magics;


ThreadControler::ThreadControler(Thread* proc, bool detached) :
    detached_(detached),
#ifndef MAGICS_ON_WINDOWS
    thread_(0),
#else
    thread_({{0}, {0}}),
#endif
    proc_(proc),
    running_(false) {
}

ThreadControler::~ThreadControler() {
    AutoLock<MutexCond> lock(cond_);

    if (running_) {
        // The Thread will delete itself
        // so there is no need for:
        // delete proc_;
    }
    else {
        delete proc_;
    }
}

//------------------------------------------------------

void ThreadControler::execute() {
    static const char* here = __FUNCTION__;

    //=================
    // Make sure the logs are created...


    //============

    Thread* proc = proc_;

    {  // Signal that we are running

        AutoLock<MutexCond> lock(cond_);
        running_ = true;
        cond_.signal();
    }

    //=============

    // We don't want to recieve reconfigure events

#ifndef MAGICS_ON_WINDOWS
    sigset_t set, old_set;

    sigemptyset(&set);

    sigaddset(&set, SIGHUP);
    sigaddset(&set, SIGCHLD);
    sigaddset(&set, SIGPIPE);

#ifdef IBM
    SYSCALL(sigthreadmask(SIG_BLOCK, &set, &old_set));
#else
    SYSCALL(pthread_sigmask(SIG_BLOCK, &set, &old_set));
#endif
#endif

    //=============

    try {
        proc->run();
    }
    catch (MagException& e) {
        magics::MagLog::error() << "** " << e.what() << " Caught in " << here << endl;
#ifndef MAGICS_ON_WINDOWS
        magics::MagLog::error() << "** MagException is termiates thread " << pthread_self() << endl;
#else
        pthread_t pt = pthread_self();
        magics::MagLog::error() << "** MagException is termiates thread " << pt.p << pt.x << endl;
#endif
    }
    catch (...) {
        magics::MagLog::error() << "** UNKNOWN MagException Caught in " << here << endl;
#ifndef MAGICS_ON_WINDOWS
        magics::MagLog::error() << "** MagException is termiates thread " << pthread_self() << endl;
#else
        pthread_t pt = pthread_self();
        magics::MagLog::error() << "** MagException is termiates thread " << pt.p << pt.x << endl;
#endif
    }

    if (proc->autodel_)
        delete proc;
}

void* ThreadControler::startThread(void* data) {
    ((ThreadControler*)data)->execute();  // static_cast or dynamic_cast ??
    return 0;
}

void ThreadControler::start() {
#ifndef MAGICS_ON_WINDOWS
    ASSERT(thread_ == 0);
#else
    ASSERT(thread_.p == 0);
    ASSERT(thread_.x == 0);
#endif

    pthread_attr_t attr;
    pthread_attr_init(&attr);

#ifdef linux
    proc_->data_ = 0;

    size_t size = 2 * 1024 * 1024;

    if (!getEnvVariable("MAGPLUS_STACK_SIZE").empty()) {
        size = tonumber(getEnvVariable("MAGPLUS_STACK_SIZE"));
        MagLog::warning() << "MAGPLUS_STACK_SIZE jas been set to " << size << endl;
    }

    pthread_attr_setstacksize(&attr, size);
#endif

    if (detached_)
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    else
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

    AutoLock<MutexCond> lock(cond_);

#ifdef DCE_THREADS
    THRCALL(pthread_create(&thread_, attr, startThread, this));
#else
    THRCALL(pthread_create(&thread_, &attr, startThread, this));
#endif

    pthread_attr_destroy(&attr);

    while (!running_)
        cond_.wait();
}

void ThreadControler::kill() {
    pthread_cancel(thread_);
}

void ThreadControler::stop() {
    proc_->stop();
}

void ThreadControler::wait() {
    ASSERT(!detached_);
    THRCALL(pthread_join(thread_, 0));
}

bool ThreadControler::active() {
#ifndef MAGICS_ON_WINDOWS
    if (thread_ != 0)
#else
    if (thread_.p != 0)
#endif
    {
        // Try see if it exists

        int policy;
        sched_param param;

        int n = pthread_getschedparam(thread_, &policy, &param);

        // The thread does not exist
        if (n != 0) {
#ifndef MAGICS_ON_WINDOWS
            thread_ = 0;
#else
            thread_.p = 0;
            thread_.x = 0;
#endif
        }
    }
#ifndef MAGICS_ON_WINDOWS
    return thread_ != 0;
#else
    return thread_.p != 0;
#endif
}
