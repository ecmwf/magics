/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MagYaml.h"
#include "MagicsGlobal.h"
using namespace magics;

int main(int argc, char** argv) {
    // MagicsGlobal::strict(true);

    try {
        for (int i = 1; i < argc; i++) {
            MagYaml::execute(argv[i]);
        }
    }
    catch (std::exception& e) {
        std::cerr << "Magics: terminated with exception: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
