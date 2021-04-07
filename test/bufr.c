/*
 * (C) Copyright 2021- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdio.h>
#include "magics_api.h"

int main() {

    mag_open();
    mag_setc("output_format", "ps");
    mag_setc("output_name", "bufr_c");

    /* load the data */
    mag_setc("obs_input_type", "file");
    mag_setc("obs_input_file_name", "synop.bufr");

    mag_coast();
    mag_obs();
    mag_text();

    /* close magics and trigger plot generation */
    mag_close();
    return 0;
}
