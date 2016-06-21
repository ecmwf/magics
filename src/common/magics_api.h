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

#ifdef __cplusplus
extern "C" {
#endif

void mag_open();
void mag_close();
void mag_coast();
void mag_grib();
void mag_mapgen();
void mag_test();
void mag_odb();
void mag_bufr();
void mag_legend();
void mag_import();
void mag_netcdf();
void mag_cont();
void mag_obs();
void mag_raw();
void mag_image();
void mag_plot();
void mag_text();
void mag_wind();
void mag_symb();
void mag_boxplot();
void mag_line();
void mag_taylor();
void mag_tephi();
void mag_input();
void mag_table();
void mag_geojson();
void mag_wrepjson();
void mag_epsinput();
void mag_epscloud();
void mag_epsplumes();
void mag_epsgraph();
void mag_epsbar();
void mag_epsshading();
void mag_epswind();
void mag_epswave();
void mag_metgraph();
void mag_metbufr();

void mag_new(const char* page);

void mag_setc(const char* name, const char*  value);
void mag_setr(const char* name, const double value);
void mag_seti(const char* name, const int value);
void mag_setp(const char* name, void* value);
void mag_reset(const char* name);

void mag_act  (const char* name, const char*, const char*);
void mag_set1i(const char* name, const int* data, const int dim1);
void mag_set2i(const char* name, const int* data, const int dim1, const int dim2);
void mag_set3i(const char* name, const int* data, const int dim1, const int dim2, const int dim3);
void mag_set1c(const char* name, const char** value, const int dim1);
void mag_set1r(const char* name, const double* data, const int dim1);
void mag_set2r(const char* name, const double* data, const int dim1, const int dim2);
void mag_set3r(const char* name, const double* data, const int dim1, const int dim2, const int dim3);

void mag_enqr (const char* name, double* value);
void mag_enqi (const char* name, int* value);
void mag_enqc (const char* name, char* value);

void mag_pie();
void mag_graph();
void mag_axis();
void mag_geo();
void mag_eps();
void mag_print();
void mag_info();

/* Definition of the python web interface*/
void execute_magml(const char*);
void execute_json(const char*);
void set_param(const char*, const char*);


#ifdef __cplusplus
}
#endif

#endif
