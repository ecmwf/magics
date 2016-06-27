/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "magics_api.h"
#include <stdio.h>

int main()
{

        mag_open();
        mag_setc ("output_format","ps");
        mag_setc ("output_name","testc"); // which is different each time

        mag_coast();
        mag_close();
        return 0;
}
