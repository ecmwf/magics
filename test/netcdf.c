/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdio.h>
#include "magics_api.h"

#define NUM_FORMATS 3


void myprint_error(void* data, const char* msg) {
    printf("ERROR:%s", msg);
}

void myprint_info(void* data, const char* msg) {
    printf("INFO:%s", msg);
}

void myprint_warning(void* data, const char* msg) {
    printf("WARNING:%s", msg);
}

void myprint_warning2(void* data, const char* msg) {
    printf("WARNING2:%s", msg);
}

void myprint_debug(void* data, const char* msg) {
    printf("DEBUG:%s", msg);
}

int main() {
    int data = 10;

    mag_add_warning_listener(&data, myprint_warning);
    mag_add_error_listener(&data, myprint_error);
    mag_open();
    const char* formats[NUM_FORMATS];
    formats[0] = "ps";
    formats[1] = "png";
    formats[2] = "pdf";

    mag_set1c("output_formats", formats, NUM_FORMATS);

    mag_setc("output_name", "contour_netcdf");  // which is different each time

    /* load the data */
    mag_setc("netcdf_type", "geomatrix");
    mag_setc("netcdf_filename", "era5_2mt.nc");
    mag_setc("netcdf_value_variable", "t2m");
    mag_netcdf();

    /* define the contouring parameters */
    mag_setc("contour", "on");
    mag_setc("contour_line_colour", "sky");
    mag_setc("CONTOUR_HIGHLIGHT_COLOUR", "GREEN");
    mag_setc("contour_label", "on");
    mag_cont();

    mag_coast();

    // mag_clear_listeners();
    /* close magics and trigger plot generation */
    mag_close();
    return 0;
}
