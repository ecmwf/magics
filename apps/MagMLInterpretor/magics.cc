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
#include "MagLog.h"


using namespace magics;

static void usage() {
    std::cerr << "Usage: magics [--nostrict] [--silent] [--debug] [--compatibility] file1.yaml [file2.yaml ...]"
              << std::endl;
}
int main(int argc, char** argv) {
    MagicsGlobal::strict(true);

    int n = 1;
    for (; n < argc; n++) {
        if (argv[n][0] != '-') {
            break;
        }

        std::string a(argv[n]);

        if (a == "--") {
            n++;
            break;
        }

        if (a == "--nostrict") {
            MagicsGlobal::strict(false);
            continue;
        }

        if (a == "--compatibility") {
            MagicsGlobal::compatibility(true);
            continue;
        }

        if (a == "--silent") {
            MagicsGlobal::silent(true);
            continue;
        }

        if (a == "--debug") {
            MagLog::debugMessage(true);
            continue;
        }

         if (a == "--dev") {
            MagLog::devMessage(true);
            continue;
        }

        if (a == "--profiling") {
            MagLog::profilingMessage(true);
            continue;
        }

        std::cerr << "Unkown option '" << a << "'" << std::endl;
        usage();
        return 1;
    }

    if (n == argc) {
        usage();
        return 1;
    }

    try {
        for (; n < argc; n++) {
            MagYaml::execute(argv[n]);
        }
    }
    catch (std::exception& e) {
        std::cerr << "Magics: input file: " << argv[n] << std::endl;
        std::cerr << "Magics: terminated with exception: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
