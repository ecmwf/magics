/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "magics_windef.h"
#ifndef MAGICS_ON_WINDOWS
#include <unistd.h>
#endif

#include <signal.h>
#include "MetaData.h"
#include "WebFormat.h"

#ifdef  HAVE_ODB 
#include <eckit/runtime/Main.h>
#endif

using namespace magics;

namespace magml {

pair<string, string> cut(const string& in) {
    string::const_iterator c = in.begin();
    string var;
    string current;
    bool param = true;

    while (c != in.end()) {
        if (*c == '=' && param) {
            var     = current;
            current = "";
            param   = false;
        }
        else {
            current.push_back(*c);
        }
        c++;
    }
    return std::make_pair(var, current);
}

void setVariable(const string& in, map<string, string>& variables) {
    if (in[0] != '-') {
        MagLog::warning() << "argument " << in << " ignored." << std::endl;
        return;
    }
    variables.insert(magml::cut(in.substr(1, in.size())));
}

}  // end of namespace magml

void catch_alarm(int) {
    printf("MagPlus ERROR: Operation timed out. Exiting...\n");
    abort();
}

int normal_main(int argc, char** argv) {


    try {
        MetaDataVisitor::start();
        if (argc < 2) {
            std::cout << getMagicsVersionString() << endl;
            std::cout << "Usage: " << argv[0] << " file.json [-varname=varvalue]" << endl;
            exit(1);
        }

        map<string, string> variables;

        for (int i = 2; i < argc; i++)
            magml::setVariable(argv[i], WebInterpretor::parameters());

        // for ( int i = 0; i < 10; i++)
        {
            try {
                map<string, string>::const_iterator out = WebInterpretor::parameters().find("timeout");
                int timeout = (out == WebInterpretor::parameters().end()) ? 0 : tonumber(out->second);
                MagLog::info() << "Time out armed for " << timeout << endl;
#ifndef MAGICS_ON_WINDOWS
                signal(SIGALRM, catch_alarm);
                alarm(timeout);
#endif
                WebInterpretor::json(argv[1]);
#ifndef MAGICS_ON_WINDOWS
                alarm(0);
#endif
            }
            catch ( MagicsException& e ) {
                std::cout << argv[0] << " FAILED to dispatch JSON file!" << e << endl;
                exit(1);
            }
        }
    }
    catch (MagicsException& e) {
        std::cout << "MagJson: Catch Exception " << e << endl;
        exit(1);
    }
    return 0;
}

int server_main(int argc, char** argv) {


    char line[10240];
    cout << "MAGJSON_SERVER_MODE Ready" << endl;
    for (;;) {
        cin.getline(line, sizeof(line));
        cout << "Running " << line << endl;
        try {
            MetaDataVisitor::start();
            WebInterpretor::json(line);
        }
        catch (...) {
            std::cout << argv[0] << " FAILED to dispatch JSON file!" << endl;
            cout << "CODE 1" << endl << flush;
            exit(1);
        }
        cout << "CODE 0" << endl << flush;
    }
    return 0;
}

int main(int argc, char** argv) {
#ifdef  HAVE_ODB 
    eckit::Main::initialise(argc, argv);
#endif

    if (getenv("MAGJSON_SERVER_MODE")) {
        server_main(argc, argv);
    }
    else {
        normal_main(argc, argv);
    }
    exit(0);
}
