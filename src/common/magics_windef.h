/*
 * Copyright 2005-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
 * virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
 */

#ifndef magics_windef_H
#define magics_windef_H

/* Microsoft Windows Visual Studio support */
#if defined(_WIN32) && defined(_MSC_VER)
#define MAGICS_ON_WINDOWS
/*
 * <algorithm> is required to use std::min() and std::max() on Windows. We could add it
 * to each file as required, but it's more straightforward to put it here.
 */
#include <algorithm>
#include <functional>  // for std::greater
#ifndef M_PI
#define M_PI 3.14159265358979323846
#define M_PI_2 1.57079632679489661923
#endif
#endif

#endif
