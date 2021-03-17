/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Units.cc
    \brief Implementation of Units class.


    Changes:


*/
#include "Units.h"
#include <map>

#include "MagException.h"
#include "MagLog.h"
#include "MagParser.h"
#include "MagicsSettings.h"

using namespace magics;


struct Scaling {
    double scaling_;
    double offset_;
    Scaling(double scaling = 1, double offset = 0) : scaling_(scaling), offset_(offset) {}
};


static std::map<std::pair<std::string, std::string>, Scaling> conversions;
static std::map<std::string, std::string> preferred;

static void init() {
    if (conversions.empty()) {
        std::string path  = buildConfigPath("units-conversions.yaml");
        ValueList entries = MagParser::decodeFile(path);

        for (auto& e : entries) {
            // MagLog::warning()  << e << std::endl;
            std::string from = e["from"];
            std::string to   = e["to"];
            double scaling   = e["scaling"];
            double offset    = e["offset"];

            auto p = std::make_pair(from, to);
            if (conversions.find(p) != conversions.end()) {
                MagLog::error() << "Unit mapping [" << from << "] => [" << to << "] detected more than once"
                                << std::endl;
                // throw MagicsException("Unit " + from + " already defined");
            }

            conversions[p] = Scaling(scaling, offset);
        }
    }
    if (preferred.empty()) {
        std::string path  = buildConfigPath("units-plotting.yaml");
        ValueList entries = MagParser::decodeFile(path);

        for (auto& e : entries) {
            std::string data = e["data"];
            std::string plot = e["plot"];

            if (preferred.find(data) != preferred.end()) {
                MagLog::error() << "Unit [" + data + "] detected more than once" << std::endl;
            }

            preferred[data] = plot;
        }
    }
}

bool Units::convert(const std::string& from, const std::string& to, double& scaling, double& offset) {
    scaling = 1;
    offset  = 0;

    init();

    if (to.empty()) {
        MagLog::dev() << "No unit conversion needed: " << from << std::endl;

        return false;
    }

    if (from == "~") {
        // FIXME:
        MagLog::warning() << "Cannot convert data to " << to << ", data units not known" << std::endl;
        return false;
    }

    if (from.empty()) {
        if (MagicsSettings::strict()) {
            throw MagicsException("Cannot convert data to " + to + ", data units not known");
        }
        MagLog::warning() << "Cannot convert data to " << to << ", data units not known" << std::endl;
        return false;
    }

    if (from == to) {
        MagLog::dev() << "++++++ Units " << from << " to " << to << " scaling " << scaling << " offset " << offset
                      << std::endl;
        return false;
    }

    auto j = conversions.find(std::make_pair(from, to));
    if (j != conversions.end()) {
        scaling = (*j).second.scaling_;
        offset  = (*j).second.offset_;
        MagLog::dev() << "++++++ Units " << from << " to " << to << " scaling " << scaling << " offset " << offset
                      << std::endl;
        return true;
    }

    // y = a * x + b
    // x = (y - b)/a

    j = conversions.find(std::make_pair(to, from));
    if (j != conversions.end()) {
        scaling = 1.0 / (*j).second.scaling_;
        offset  = -(*j).second.offset_ / (*j).second.scaling_;
        MagLog::warning() << "++++++ Units (reversed) " << from << " to " << to << " scaling " << scaling << " offset "
                          << offset << std::endl;

        return true;
    }

    if (MagicsSettings::strict()) {
        throw MagicsException("Cannot convert data from [" + from + "] to [" + to + "]");
    }
    MagLog::warning() << "Cannot convert data from [" << from << "] to [" << to << "]" << std::endl;
    return false;
}

void Units::defaultScaling(double& scaling, double& offset, std::string& dataUnits, std::string& plotUnits) {
    init();

    scaling   = 1;
    offset    = 0;
    plotUnits = dataUnits;

    auto j = preferred.find(dataUnits);
    if (j != preferred.end()) {
        plotUnits = (*j).second;
        ASSERT(convert(dataUnits, plotUnits, scaling, offset));
    }
}
