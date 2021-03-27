/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BaseParameter.cc
    \brief Definition of Parameter base class.

    Magics Team - ECMWF 2004

    Started: Jan 2004

    Changes:

*/


#include "BaseParameter.h"
#include "ParameterManager.h"

using namespace magics;


BaseParameter::BaseParameter(const string& name) : name_(name) {
    ParameterManager::add(name_, this);
}


BaseParameter::~BaseParameter() {}


void BaseParameter::print(ostream& out) const {
    out << name_ << "[]";
}

//=================================
static std::string unknown = "unknown";

static map<string, LineStyle> _LineStyle = {
    {
        "solid",
        LineStyle::SOLID,
    },
    {
        "dash",
        LineStyle::DASH,
    },
    {
        "dot",
        LineStyle::DOT,
    },
    {
        "chain_dash",
        LineStyle::CHAIN_DASH,
    },
    {
        "chain_dot",
        LineStyle::CHAIN_DOT,
    },
};

LineStyle BaseParameter::lineStyle(const std::string& name) {
    auto j = _LineStyle.find(name);
    if (j == _LineStyle.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a LineStyle,"
                          << " changed to 'solid'" << endl;
        return LineStyle::SOLID;
    }
    return (*j).second;
}

const std::string& BaseParameter::lineStyle(const LineStyle& x) {
    for (auto j = _LineStyle.begin(); j != _LineStyle.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, LineStyle x) {
    s << BaseParameter::lineStyle(x);
    return s;
}

//=================================

static map<string, ListPolicy> _ListPolicy = {
    {
        "lastone",
        ListPolicy::LASTONE,
    },
    {
        "cycle",
        ListPolicy::CYCLE,
    },

};

ListPolicy BaseParameter::listPolicy(const std::string& name) {
    auto j = _ListPolicy.find(name);
    if (j == _ListPolicy.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a ListPolicy,"
                          << " changed to 'lastone'" << endl;
        return ListPolicy::LASTONE;
    }
    return (*j).second;
}

const std::string& BaseParameter::listPolicy(const ListPolicy& x) {
    for (auto j = _ListPolicy.begin(); j != _ListPolicy.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, ListPolicy x) {
    s << BaseParameter::listPolicy(x);
    return s;
}


//=================================

static map<string, DisplayType> _DisplayType = {
    {
        "absolute",
        DisplayType::ABSOLUTE,
    },
    {
        "inline",
        DisplayType::INLINE,
    },
    {
        "block",
        DisplayType::BLOCK,
    },
    {
        "none",
        DisplayType::NONE,
    },
    {
        "hidden",
        DisplayType::HIDDEN,
    },

};

DisplayType BaseParameter::displayType(const std::string& name) {
    auto j = _DisplayType.find(name);
    if (j == _DisplayType.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a DisplayType,"
                          << " changed to 'absolute'" << endl;
        return DisplayType::ABSOLUTE;
    }
    return (*j).second;
}

const std::string& BaseParameter::displayType(const DisplayType& x) {
    for (auto j = _DisplayType.begin(); j != _DisplayType.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, DisplayType x) {
    s << BaseParameter::displayType(x);
    return s;
}


//=================================

static map<string, Justification> _Justification = {
    {
        "left",
        Justification::LEFT,
    },
    {
        "centre",
        Justification::CENTRE,
    },
    {
        "right",
        Justification::RIGHT,
    },

};

Justification BaseParameter::justification(const std::string& name) {
    auto j = _Justification.find(name);
    if (j == _Justification.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a Justification,"
                          << " changed to 'centre'" << endl;
        return Justification::CENTRE;
    }
    return (*j).second;
}

const std::string& BaseParameter::justification(const Justification& x) {
    for (auto j = _Justification.begin(); j != _Justification.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, Justification x) {
    s << BaseParameter::justification(x);
    return s;
}


//=================================

static map<string, AxisAutomaticSetting> _AxisAutomaticSetting = {
    {
        "off",
        AxisAutomaticSetting::OFF,
    },
    {
        "no",
        AxisAutomaticSetting::OFF,
    },
    {
        "false",
        AxisAutomaticSetting::OFF,
    },
    {
        "on",
        AxisAutomaticSetting::BOTH,
    },
    {
        "yes",
        AxisAutomaticSetting::BOTH,
    },
    {
        "true",
        AxisAutomaticSetting::BOTH,
    },
    {
        "both",
        AxisAutomaticSetting::BOTH,
    },
    {
        "min_only",
        AxisAutomaticSetting::MIN_ONLY,
    },
    {
        "max_only",
        AxisAutomaticSetting::MAX_ONLY,
    },
};

AxisAutomaticSetting BaseParameter::axisAutomaticSetting(const std::string& name) {
    auto j = _AxisAutomaticSetting.find(name);
    if (j == _AxisAutomaticSetting.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a AxisAutomaticSetting,"
                          << " changed to 'off'" << endl;
        return AxisAutomaticSetting::OFF;
    }
    return (*j).second;
}

const std::string& BaseParameter::axisAutomaticSetting(const AxisAutomaticSetting& x) {
    for (auto j = _AxisAutomaticSetting.begin(); j != _AxisAutomaticSetting.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, AxisAutomaticSetting x) {
    s << BaseParameter::axisAutomaticSetting(x);
    return s;
}

//=================================

static map<string, Position> _Position = {
    {
        "automatic",
        Position::AUTOMATIC,
    },
    {
        "left",
        Position::LEFT,
    },
    {
        "right",
        Position::RIGHT,
    },
    {
        "bottom",
        Position::BOTTOM,
    },
    {
        "top",
        Position::TOP,
    },
};

Position BaseParameter::position(const std::string& name) {
    auto j = _Position.find(name);
    if (j == _Position.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a Position,"
                          << " changed to 'automatic'" << endl;
        return Position::AUTOMATIC;
    }
    return (*j).second;
}

const std::string& BaseParameter::position(const Position& x) {
    for (auto j = _Position.begin(); j != _Position.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, Position x) {
    s << BaseParameter::position(x);
    return s;
}

//=================================

static map<string, Hemisphere> _Hemisphere = {
    {
        "north",
        Hemisphere::NORTH,
    },
    {
        "south",
        Hemisphere::SOUTH,
    },
};

Hemisphere BaseParameter::hemisphere(const std::string& name) {
    auto j = _Hemisphere.find(name);
    if (j == _Hemisphere.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a Hemisphere,"
                          << " changed to 'north'" << endl;
        return Hemisphere::NORTH;
    }
    return (*j).second;
}

const std::string& BaseParameter::hemisphere(const Hemisphere& x) {
    for (auto j = _Hemisphere.begin(); j != _Hemisphere.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, Hemisphere x) {
    s << BaseParameter::hemisphere(x);
    return s;
}


//=================================

static map<string, ArrowPosition> _ArrowPosition = {
    {
        "tail",
        ArrowPosition::TAIL,
    },
    {
        "centre",
        ArrowPosition::CENTRE,
    },
    {
        "head_only",
        ArrowPosition::HEAD_ONLY,
    },
};

ArrowPosition BaseParameter::arrowPosition(const std::string& name) {
    auto j = _ArrowPosition.find(name);
    if (j == _ArrowPosition.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a ArrowPosition,"
                          << " changed to 'tail'" << endl;
        return ArrowPosition::TAIL;
    }
    return (*j).second;
}

const std::string& BaseParameter::arrowPosition(const ArrowPosition& x) {
    for (auto j = _ArrowPosition.begin(); j != _ArrowPosition.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, ArrowPosition x) {
    s << BaseParameter::arrowPosition(x);
    return s;
}


//=================================

static map<string, VerticalAlign> _VerticalAlign = {
    {
        "normal",
        VerticalAlign::NORMAL,
    },
    {
        "top",
        VerticalAlign::TOP,
    },
    {
        "cap",
        VerticalAlign::CAP,
    },
    {
        "half",
        VerticalAlign::HALF,
    },
    {
        "base",
        VerticalAlign::BASE,
    },
    {
        "bottom",
        VerticalAlign::BOTTOM,
    },
};

VerticalAlign BaseParameter::verticalAlign(const std::string& name) {
    auto j = _VerticalAlign.find(name);
    if (j == _VerticalAlign.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a VerticalAlign,"
                          << " changed to 'half'" << endl;
        return VerticalAlign::HALF;
    }
    return (*j).second;
}

const std::string& BaseParameter::verticalAlign(const VerticalAlign& x) {
    for (auto j = _VerticalAlign.begin(); j != _VerticalAlign.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, VerticalAlign x) {
    s << BaseParameter::verticalAlign(x);
    return s;
}


//=================================

static map<string, GraphicsFormat> _GraphicsFormat = {
    {
        "ps",
        GraphicsFormat::PS,
    },
    {
        "eps",
        GraphicsFormat::EPS,
    },
    {
        "pdf",
        GraphicsFormat::PDF,
    },
    {
        "svg",
        GraphicsFormat::SVG,
    },
    {
        "kml",
        GraphicsFormat::KML,
    },
    {
        "x",
        GraphicsFormat::X,
    },
    {
        "cps",
        GraphicsFormat::CPS,
    },
    {
        "csvg",
        GraphicsFormat::CSVG,
    },
    {
        "gif",
        GraphicsFormat::GIF,
    },
    {
        "agif",
        GraphicsFormat::AGIF,
    },
    {
        "jpg",
        GraphicsFormat::JPG,
    },
    {
        "qt",
        GraphicsFormat::QT,
    },
    {
        "geojson",
        GraphicsFormat::GEOJSON,
    },
};

GraphicsFormat BaseParameter::graphicsFormat(const std::string& name) {
    auto j = _GraphicsFormat.find(name);
    if (j == _GraphicsFormat.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a GraphicsFormat,"
                          << " changed to 'png'" << endl;
        return GraphicsFormat::PNG;
    }
    return (*j).second;
}

const std::string& BaseParameter::graphicsFormat(const GraphicsFormat& x) {
    for (auto j = _GraphicsFormat.begin(); j != _GraphicsFormat.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, GraphicsFormat x) {
    s << BaseParameter::graphicsFormat(x);
    return s;
}
