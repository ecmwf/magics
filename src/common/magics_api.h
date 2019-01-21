/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file magics_api.h
  \brief Implementation of C interface
  This header file needs to be included by all C programs
  using the C Magics parameter interface.

  Be careful with including stdio.h because
  pclose() and popen() are used there too!!!
*/
#ifndef _MAGICS_API_H_
#define _MAGICS_API_H_

#include "magics_export.h"

#ifdef __cplusplus
extern "C" {
#endif

MAGICS_EXPORT void mag_open();
MAGICS_EXPORT int mag_close();
MAGICS_EXPORT void mag_coast();
MAGICS_EXPORT void mag_grib();
MAGICS_EXPORT void mag_mapgen();
MAGICS_EXPORT void mag_test();
MAGICS_EXPORT void mag_odb();
MAGICS_EXPORT void mag_bufr();
MAGICS_EXPORT void mag_legend();
MAGICS_EXPORT void mag_import();
MAGICS_EXPORT void mag_netcdf();
MAGICS_EXPORT void mag_cont();
MAGICS_EXPORT void mag_obs();
MAGICS_EXPORT void mag_raw();
MAGICS_EXPORT void mag_image();
MAGICS_EXPORT void mag_plot();
MAGICS_EXPORT void mag_text();
MAGICS_EXPORT void mag_wind();
MAGICS_EXPORT void mag_symb();
MAGICS_EXPORT void mag_boxplot();
MAGICS_EXPORT void mag_line();
MAGICS_EXPORT void mag_taylor();
MAGICS_EXPORT void mag_tephi();
MAGICS_EXPORT void mag_input();
MAGICS_EXPORT void mag_table();
MAGICS_EXPORT void mag_geojson();
MAGICS_EXPORT void mag_wrepjson();
MAGICS_EXPORT void mag_epsinput();
MAGICS_EXPORT void mag_epscloud();
MAGICS_EXPORT void mag_epsplumes();
MAGICS_EXPORT void mag_epsgraph();
MAGICS_EXPORT void mag_epslight();

MAGICS_EXPORT void mag_epsbar();
MAGICS_EXPORT void mag_epsshading();
MAGICS_EXPORT void mag_epswind();
MAGICS_EXPORT void mag_epswave();
MAGICS_EXPORT void mag_metgraph();
MAGICS_EXPORT void mag_metbufr();

MAGICS_EXPORT void mag_new(const char* page);

MAGICS_EXPORT void mag_setc(const char* name, const char* value);
MAGICS_EXPORT void mag_setr(const char* name, const double value);
MAGICS_EXPORT void mag_seti(const char* name, const int value);
MAGICS_EXPORT void mag_setp(const char* name, void* value);
MAGICS_EXPORT void mag_reset(const char* name);

MAGICS_EXPORT void mag_act(const char* name, const char*, const char*);
MAGICS_EXPORT void mag_set1i(const char* name, const int* data, const int dim1);
MAGICS_EXPORT void mag_set2i(const char* name, const int* data, const int dim1, const int dim2);
MAGICS_EXPORT void mag_set3i(const char* name, const int* data, const int dim1, const int dim2, const int dim3);
MAGICS_EXPORT void mag_set1c(const char* name, const char** value, const int dim1);
MAGICS_EXPORT void mag_set1r(const char* name, const double* data, const int dim1);
MAGICS_EXPORT void mag_set2r(const char* name, const double* data, const int dim1, const int dim2);
MAGICS_EXPORT void mag_set3r(const char* name, const double* data, const int dim1, const int dim2, const int dim3);

MAGICS_EXPORT void mag_enqr(const char* name, double* value);
MAGICS_EXPORT void mag_enqi(const char* name, int* value);
MAGICS_EXPORT void mag_enqc(const char* name, char* value);

MAGICS_EXPORT void mag_pie();
MAGICS_EXPORT void mag_graph();
MAGICS_EXPORT void mag_axis();
MAGICS_EXPORT void mag_geo();
MAGICS_EXPORT void mag_eps();
MAGICS_EXPORT void mag_print();
MAGICS_EXPORT void mag_info();

/* Definition of the python web interface*/
MAGICS_EXPORT void execute_magml(const char*);
MAGICS_EXPORT void execute_json(const char*);
MAGICS_EXPORT void set_param(const char*, const char*);


#ifdef __cplusplus
}
#endif

#endif
