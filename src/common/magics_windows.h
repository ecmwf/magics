/*
 * (C) Copyright 1996-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdio.h>

#include <pthread.h>

bool operator<(const pthread_t& left, const pthread_t& right);

#include <errno.h>

#include <string.h>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <new>


#include <functional>
#include <sstream>

// Stl

#include <algorithm>
#include <list>
#include <map>
#include <memory>
#include <numeric>
#include <queue>
#include <set>
#include <stack>
#include <string>
#include <vector>

using namespace std;

#define THROW_NOTHING throw

#ifndef magics_windows
#define magics_windows
#endif
