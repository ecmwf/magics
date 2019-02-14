/*
 * (C) Copyright 1996-2019 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

// do the export restriction only if NOT for Metview
#ifndef HAVE_METVIEW

#ifdef WIN32
#define MAGICS_NO_EXPORT
#define MAGICS_IMPORT __declspec(dllimport)
#define MAGICS_EXPORT __declspec(dllexport)
#define MAGICS_DLLLOCAL
#define MAGICS_DLLPUBLIC
#elif (__GNUC__ - 0 > 3) || (__GNUC__ - 0 == 3 && __GNUC_MINOR__ - 0 > 2)
#define MAGICS_GCC 4
#define MAGICS_NO_EXPORT __attribute__((visibility("hidden")))
#define MAGICS_EXPORT __attribute__((visibility("default")))
#define MAGICS_IMPORT __attribute__((visibility("default")))
#define MAGICS_DLLLOCAL __attribute__((visibility("hidden")))
#define MAGICS_DLLPUBLIC __attribute__((visibility("default")))
#else
#define MAGICS_GCC 3
#define MAGICS_IMPORT
#define MAGICS_EXPORT
#define MAGICS_DLLLOCAL
#define MAGICS_DLLPUBLIC
#endif

#else
#define MAGICS_GCC 3
#define MAGICS_NO_EXPORT
#define MAGICS_IMPORT
#define MAGICS_EXPORT
#define MAGICS_DLLLOCAL
#define MAGICS_DLLPUBLIC
#endif
