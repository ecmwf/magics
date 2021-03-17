/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <fstream>
#include "magics.h"

#include "MagYaml.h"

#include "JSON.h"
#include "MagException.h"
#include "MagParser.h"
#include "MagicsCalls.h"
#include "MagicsSettings.h"

using namespace magics;

typedef void (*action_proc)();

static std::map<std::string, action_proc> actions = {
    {"maxis", MagicsCalls::axis},
    {"mboxplot", MagicsCalls::boxplot},
    {"mcoast", MagicsCalls::coast},
    {"mcont", MagicsCalls::cont},
    {"mepsbar", MagicsCalls::epsbar},
    {"mepscloud", MagicsCalls::epscloud},
    {"mepsgraph", MagicsCalls::epsgraph},
    {"mepsinput", MagicsCalls::epsinput},
    {"mepslight", MagicsCalls::epslight},
    {"mepsplumes", MagicsCalls::epsplumes},
    {"mepsshading", MagicsCalls::epsshading},
    {"mepswave", MagicsCalls::epswave},
    {"mepswind", MagicsCalls::epswind},
    {"mgeo", MagicsCalls::geo},
    {"mgeojson", MagicsCalls::geojson},
    {"mgraph", MagicsCalls::graph},
    {"mgrib", MagicsCalls::grib},
    {"mimage", MagicsCalls::image},
    {"mimport", MagicsCalls::import},
    {"minput", MagicsCalls::input},
    {"mlegend", MagicsCalls::legend},
    {"mline", MagicsCalls::line},
    {"mmap", nullptr},
    {"mmapgen", MagicsCalls::mapgen},
    {"mmetbufr", MagicsCalls::metbufr},
    {"mmetgraph", MagicsCalls::metgraph},
    {"mnetcdf", MagicsCalls::netcdf},
    {"mobs", MagicsCalls::obs},
    {"modb", MagicsCalls::odb},
    {"moverlay", MagicsCalls::overlay},
    {"mraw", MagicsCalls::raw},
    {"msymb", MagicsCalls::symb},
    {"mtable", MagicsCalls::table},
    {"mtaylor", MagicsCalls::taylor},
    {"mtephi", MagicsCalls::tephi},
    {"mtext", MagicsCalls::text},
    {"mtile", MagicsCalls::tile},
    {"mwind", MagicsCalls::wind},
    {"mwrepjson", MagicsCalls::wrepjson},
    {"output", nullptr},
    {"page", MagicsCalls::new_page},
    {"page", MagicsCalls::new_page},

};

// TODO: NO globals!!!

static std::map<std::string, std::set<std::string> > reset;

static void execute(const std::string& action, const Value& p) {
    auto k = actions.find(action);
    if (k == actions.end()) {
        throw MagicsException("Unknown action: " + action);
    }

    // Reset previous settings
    for (auto r : reset[action]) {
        MagicsCalls::reset(r);
    }
    reset[action].clear();

    action_proc proc = (*k).second;


    ValueMap param = p;
    for (auto j = param.begin(); j != param.end(); ++j) {
        std::string name = (*j).first;
        Value value      = (*j).second;

        reset[action].insert(name);

        if (value.isBool()) {
            MagicsCalls::setc(name, bool(value) ? "on" : "off");
            continue;
        }

        if (value.isString()) {
            std::string s = value;
            MagicsCalls::setc(name, s);
            continue;
        }

        if (value.isMap()) {
            ostringstream oss;
            JSON json(oss);
            json << value;
            MagicsCalls::setc(name, oss.str());
            continue;
        }

        if (value.isNumber() || value.isDouble()) {
            double d = value;
            if (long(d) == d) {
                MagicsCalls::seti(name, long(d));
            }
            else {
                MagicsCalls::setr(name, d);
            }
            continue;
        }

        if (value.isList()) {
            ValueList l = value;
            size_t s    = 0;
            size_t i    = 0;
            size_t d    = 0;
            size_t a    = 0;
            for (auto e : l) {
                if (e.isList()) {
                    a++;
                    continue;
                }
                if (e.isString()) {
                    s++;
                    continue;
                }
                if (e.isNumber() || e.isDouble()) {
                    double v = e;
                    if (int(v) == v) {
                        i++;
                    }
                    else {
                        d++;
                    }
                    continue;
                }
                std::ostringstream oss;
                oss << "Value type not supported in list: " << name << " = " << value << " e=" << e;
                throw MagicsException(oss.str());
            }
            if (a) {
                int dim = -1;
                vector<double> values;
                for (ValueList row : l) {
                    if (dim == -1) {
                        dim = row.size();
                    }
                    else {
                        ASSERT(dim == row.size());
                    }
                    for (double d : row) {
                        values.push_back(d);
                    }
                }
                MagicsCalls::set2r(name, values, dim, l.size());
            }
            else if (s) {
                std::vector<std::string> values(l.begin(), l.end());
                MagicsCalls::set1c(name, values);
            }
            else if (d) {
                std::vector<double> values(l.begin(), l.end());
                MagicsCalls::set1r(name, values);
            }
            else if (i) {
                std::vector<int> values(l.begin(), l.end());
                MagicsCalls::set1i(name, values);
            }
            continue;
        }


        std::ostringstream oss;
        oss << "Value type not supported: " << name << " = " << value;
        throw MagicsException(oss.str());
    }

    if (proc) {
        proc();
    }
}


static void plot(const std::string&, const Value& param) {
    ValueList actions = param;

    MagicsCalls::open();

    for (auto j = actions.begin(); j != actions.end(); ++j) {
        ValueMap p = (*j);
        for (auto k = p.begin(); k != p.end(); ++k) {
            execute((*k).first, (*k).second);
        }
    }

    MagicsCalls::close();
}


// Checking style files
static void styles(const std::string&, const Value& param) {
    ValueList actions = param;

    MagicsSettings::silent(true);

    MagicsCalls::open();

    for (auto j = actions.begin(); j != actions.end(); ++j) {
        ValueMap p = (*j);
        for (auto k = p.begin(); k != p.end(); ++k) {
            execute((*k).first, (*k).second);
        }
    }

    // No close == no plot
}


typedef void (*command_proc)(const std::string&, const Value&);


static std::map<std::string, command_proc> commands = {{"plot", plot}, {"magics", styles}};


void MagYaml::execute(const std::string& path) {
    ValueMap p = MagParser::decodeFile(path);
    for (auto j = p.begin(); j != p.end(); ++j) {
        std::string command = (*j).first;
        auto k              = commands.find(command);
        if (k == commands.end()) {
            throw MagicsException("Unknown command: " + command);
        }
        (*(*k).second)(command, (*j).second);
    }
}
