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
#include "MagTranslator.h"
#include "ParameterManager.h"
#include "Translator.h"

using namespace magics;


BaseParameter::BaseParameter(const string& name) : name_(name) {
    ParameterManager::add(name_, this);
}


BaseParameter::~BaseParameter() {}

void BaseParameter::set(const double& value) {
    // See first if we can accept int instead
    double int_value = value;
    if (type() == getType(int_value) && int(value) == value) {
        try {
            set(int_value);
            return;
        }
        catch (MistmatchType&) {
            // Throw the original mismatch error
        }
    }

    // See first if we can accept int instead
    std::string str_value = Translator<double, string>()(value);
    if (type() == getType(str_value)) {
        try {
            set(str_value);
            return;
        }
        catch (MistmatchType&) {
            // Throw the original mismatch error
        }
    }


    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(double& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const bool& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(bool& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const magvector<double>& values) {
    // See first if we can accept ints instead

    magvector<int> int_values;
    if (type() == getType(int_values)) {
        bool ok = true;

        for (auto v : values) {
            if (int(v) == v) {
                int_values.push_back(v);
            }
            else {
                ok = false;
            }
        }

        if (ok) {
            try {
                set(int_values);
                return;
            }
            catch (MistmatchType&) {
                // Throw the original mismatch error
            }
        }
    }

    throw MistmatchType(name_, getType(values), type());
}

void BaseParameter::get(magvector<double>& values) const {
    throw MistmatchType(name_, getType(values), type());
}

void BaseParameter::set(const int& value) {
    // See first if we can accept double instead
    double double_value = value;
    if (type() == getType(double_value)) {
        try {
            set(double(value));
            return;
        }
        catch (MistmatchType&) {
            // Throw the original mismatch error
        }
    }

    // See first if we can accept int instead
    std::string str_value = Translator<int, string>()(value);
    if (type() == getType(str_value)) {
        try {
            set(str_value);
            return;
        }
        catch (MistmatchType&) {
            // Throw the original mismatch error
        }
    }

    bool bool_value = false;
    if (type() == getType(bool_value)) {
        if (value == 0 || value == 1) {
            bool_value = value != 0;

            try {
                set(bool_value);
                return;
            }
            catch (MistmatchType&) {
                // Throw the original mismatch error
            }
        }
    }

    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(int& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const magvector<int>& values) {
    // See first if we can accept double instead

    magvector<double> double_values;
    if (type() == getType(double_values)) {
        for (auto v : values) {
            double_values.push_back(v);
        }

        try {
            set(double_values);
            return;
        }
        catch (MistmatchType&) {
            // Throw the original mismatch error
        }
    }

    throw MistmatchType(name_, getType(values), type());
}

void BaseParameter::get(magvector<int>& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const magvector<long>& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(magvector<long>& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const string& value) {
    magvector<string> values;
    if (type() == getType(values)) {
        values.push_back(value);
        try {
            set(values);
            return;
        }
        catch (MistmatchType&) {
            // Throw the original mismatch error
        }
    }

    bool bool_value = false;
    if (type() == getType(bool_value)) {
        string val = lowerCase(value);

        if (val == "true" || val == "false" || val == "on" || val == "off" || val == "yes" || val == "no" ||
            val == "1" || val == "0") {
            bool_value = MagTranslator<string, bool>()(value);

            try {
                set(bool_value);
                return;
            }
            catch (MistmatchType&) {
                // Throw the original mismatch error
            }
        }
    }

    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const char* s) {
    set(std::string(s));
}

void BaseParameter::get(string& value) const {
    throw MistmatchType(name_, "string", type());
}

void BaseParameter::set(const magvector<string>& value) {
    std::cout << "++++" << std::endl;
    throw MistmatchType(name_, getType(value), type());
}


void BaseParameter::get(magvector<string>& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const LineStyle& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(LineStyle& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const DisplayType& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(DisplayType& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const Justification& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(Justification& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const ListPolicy& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const ColourListPolicy& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(ListPolicy& value) const {
    throw MistmatchType(name_, getType(value), type());
}
void BaseParameter::get(ColourListPolicy& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const AxisAutomaticSetting& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(AxisAutomaticSetting& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const ArrowPosition& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(ArrowPosition& value) const {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::set(const Matrix& value) {
    throw MistmatchType(name_, getType(value), type());
}

void BaseParameter::get(Matrix& value) const {
    throw MistmatchType(name_, getType(value), type());
}

string BaseParameter::getType(const string&) const {
    return "string";
}

string BaseParameter::getType(const int&) const {
    return "integer";
}

string BaseParameter::getType(const double&) const {
    return "real";
}

string BaseParameter::getType(const magvector<string>&) const {
    return "array of string";
}

string BaseParameter::getType(const magvector<int>&) const {
    return "array of integer";
}

string BaseParameter::getType(const magvector<long>&) const {
    return "array of long integer";
}

string BaseParameter::getType(const magvector<double>&) const {
    return "array of real";
}

string BaseParameter::getType(const LineStyle&) const {
    return "LineStyle";
}

string BaseParameter::getType(const AxisAutomaticSetting&) const {
    return "AxisAutomaticSetting";
}

string BaseParameter::getType(const Justification&) const {
    return "Justification";
}

string BaseParameter::getType(const ArrowPosition&) const {
    return "ArrowPosition";
}

string BaseParameter::getType(const Matrix&) const {
    return "2DMatrix";
}

string BaseParameter::getType(const DisplayType&) const {
    return "DisplayType";
}

string BaseParameter::getType(const ListPolicy&) const {
    return "ListPolicy";
}
string BaseParameter::getType(const ColourListPolicy&) const {
    return "ColourListPolicy";
}


void BaseParameter::print(ostream& out) const {
    out << name_ << "[]";
}

MistmatchType::MistmatchType(const string& name, const string& type, const string& wait) :
    MagicsException("Parameter '" + name + "': type mismatch -> type received '" + type + "', expected type '" + wait +
                    "'") {}

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

static map<string, ColourListPolicy> _ColourListPolicy = {
    {
        "lastone",
        ColourListPolicy::LASTONE,
    },
    {
        "cycle",
        ColourListPolicy::CYCLE,
    },
    {
        "dynamic",
        ColourListPolicy::DYNAMIC,
    },

};

ColourListPolicy BaseParameter::colourListPolicy(const std::string& name) {
    auto j = _ColourListPolicy.find(name);
    if (j == _ColourListPolicy.end()) {
        MagLog::warning() << "Invalid value '" << name << "' for a ColourListPolicy,"
                          << " changed to 'lastone'" << endl;
        return ColourListPolicy::LASTONE;
    }
    return (*j).second;
}

const std::string& BaseParameter::colourListPolicy(const ColourListPolicy& x) {
    for (auto j = _ColourListPolicy.begin(); j != _ColourListPolicy.end(); ++j) {
        if ((*j).second == x) {
            return (*j).first;
        }
    }
    return unknown;
}

std::ostream& magics::operator<<(ostream& s, ColourListPolicy x) {
    s << BaseParameter::colourListPolicy(x);
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
