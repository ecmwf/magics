/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "magics.h"

#include "MagConfig.h"
#include "MagException.h"
#include "MagLog.h"
#include "MagParser.h"
#include "MetaData.h"
#include "Value.h"


#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 201703L) || __cplusplus >= 201703L)
#include <filesystem>
namespace fs = std::filesystem;
#else
#define _SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#endif


#include <cstring>
#include "Tokenizer.h"

using namespace magics;


MagConfigHandler::MagConfigHandler(const string& config, MagConfig& magics) {



    try {
        Value value = MagParser::decodeFile(config);

        if (value.isList()) {
            ValueList values = value.get_value<ValueList>();
            magics.callback(values);
            return;
        }
        ValueMap object = value.get_value<ValueMap>();

        for (auto entry = object.begin(); entry != object.end(); ++entry) {
            magics.callback(entry->first, entry->second);
        }
    }
    catch (std::exception& e) {
        if (MagicsGlobal::strict()) {
            throw;
        }
        MagLog::error() << "JSON error in file: " << config << ": " << e.what() << endl;
    }
}

MagConfigHandler::~MagConfigHandler() {}

void MagConfigHandler::dig(const Value&) {}

void MagConfigHandler::print(ostream& out) const {
    out << "MagConfigHandler[";
    out << "]";
}


MagConfig::MagConfig() {}

MagConfig::~MagConfig() {}

string MagConfig::convert(const Value& value) {
    std::string s(value);
    return s;
}

void StyleLibrary::callback(const ValueList& values) {
    for (unsigned int i = 0; i < values.size(); i++) {
        library_.push_back(Style());
        ValueMap object = values[i].get_value<ValueMap>();
        library_.back().set(object);
    }
}

void Style::set(ValueMap& object, Style::Match& match) {
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        match.insert(make_pair(entry->first, vector<string>()));
        if (entry->second.isList()) {
            ValueList values = entry->second.get_value<ValueList>();
            for (unsigned int i = 0; i < values.size(); i++) {
                match[entry->first].push_back(MagConfig::convert(values[i]));
            }
        }
        else {
            match[entry->first].push_back(MagConfig::convert(entry->second));
        }
    }
}

void Style::criteria(const Value& value) {
    // List of criteria
    ValueList values = value.get_value<ValueList>();

    for (unsigned int i = 0; i < values.size(); i++) {
        ValueMap object = values[i].get_value<ValueMap>();
        criteria_.push_back(Style::Match());
        set(object, criteria_.back());
    }
}
void Style::style(const Value& value) {}
void Style::name(const Value& value) {}
void Style::styles(const Value& value) {
    ValueList values = value.get_value<ValueList>();

    for (unsigned int i = 0; i < values.size(); i++) {
        // If we find a name get it from the library
        if (values[i].isMap()) {
            MagDef def;
            def.set(values[i].get_value<ValueMap>());
            // push to the library !
        }
        else
            styles_.push_back(string(values[i]));
    }
}
void Style::units(const Value& value) {
    preferedUnits_ = string(value);
}

void Style::match(const Value& value) {
    ValueMap object = value.get_value<ValueMap>();

    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        map<string, SetMethod>::iterator method = methods_.find(entry->first);
        if (method != methods_.end())
            (this->*method->second)(entry->second);
        else
            MagLog::warning() << entry->first << " is not a known keyword" << endl;
    }
}

void Style::set(const ValueMap& object) {
    if (methods_.empty()) {
        methods_["match"]          = &Style::criteria;
        methods_["prefered_units"] = &Style::units;
        methods_["styles"]         = &Style::styles;
        methods_["eccharts_layer"] = &Style::name;
        methods_["visdef"]         = &Style::style;
        methods_["scaling"]        = &Style::ignore;
    }

    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        map<string, SetMethod>::iterator method = methods_.find(entry->first);
        if (method != methods_.end())
            (this->*method->second)(entry->second);
        else
            MagLog::warning() << entry->first << " is not a known keyword" << endl;
    }
}

void StyleLibrary::callback(const string& name, const Value& value) {
    if (name == "match") {
        library_.push_back(Style());
        ValueMap object = value.get_value<ValueMap>();
        library_.back().set(object);
    }
}


void StyleLibrary::init() {
    // Now we have a variable

    string ecmwf   = buildConfigPath("styles", "ecmwf");
    string library = getEnvVariable("MAGICS_STYLE_PATH");

    if (library.empty())
        library = "ecmwf";

    Tokenizer tokenizer(":");
    vector<string> paths;
    tokenizer(library, paths);


    for (auto token = paths.begin(); token != paths.end(); ++token) {
        string path = magCompare(*token, "ecmwf") ? ecmwf : *token;

        for (auto& p : fs::directory_iterator(path)) {
            std::string full = p.path().string();

            if (p.path().extension() == ".json") {
                try {
                    if (p.path().filename() == "styles.json")
                        allStyles_.init(path, "styles.json");
                    else
                        MagConfigHandler(full, *this);
                }
                catch (std::exception& e) {
                    MagLog::error() << "Error processing " << full << ": " << e.what() << ", ignored." << std::endl;
                }
            }
        }
    }
}

void PaletteLibrary::init() {
    string library = buildConfigPath("styles", "palettes.json");
    MagConfigHandler(library, *this);
}


void Palette::values(const Value& value) {
    ValueList values = value.get_value<ValueList>();

    for (unsigned int i = 0; i < values.size(); i++) {
        colours_.push_back(MagConfig::convert(values[i]));
    }
}

void Palette::tags(const Value& value) {}

void Palette::set(const ValueMap& object) {
    if (methods_.empty()) {
        methods_["contour_shade_colour_list"] = &Palette::values;
        methods_["tags"]                      = &Palette::tags;
    }
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        map<string, SetMethod>::iterator method = methods_.find(entry->first);
        if (method != methods_.end())
            (this->*method->second)(entry->second);
        else
            MagLog::warning() << entry->first << " is not a known keyword" << endl;
    }
}
void PaletteLibrary::callback(const string& name, const Value& value) {
    Palette palette;
    palette.name_              = name;
    ValueMap object = value.get_value<ValueMap>();
    palette.set(object);
    library_.insert(make_pair(name, palette));
}

void UnitsLibrary::init() {
    string library = buildConfigPath("units-rules.json");
    MagConfigHandler(library, *this);
}


void UnitConvert::from(const Value& value) {
    from_ = string(value);
}

void UnitConvert::to(const Value& value) {
    to_ = string(value);
}
void UnitConvert::scaling(const Value& value) {
    scaling_ = double(value);
}

void UnitConvert::offset(const Value& value) {
    offset_ = double(value);
}


void UnitConvert::set(const ValueMap& object) {
    if (methods_.empty()) {
        methods_["from"]    = &UnitConvert::from;
        methods_["to"]      = &UnitConvert::to;
        methods_["scaling"] = &UnitConvert::scaling;
        methods_["offset"]  = &UnitConvert::offset;
    }
    scaling_ = 1;
    offset_  = 0;
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        map<string, SetMethod>::iterator method = methods_.find(entry->first);
        if (method != methods_.end())
            (this->*method->second)(entry->second);
        else
            MagLog::warning() << entry->first << " is not a known keyword" << endl;
    }
}
void UnitsLibrary::callback(const string& name, const Value& value) {
    library_.insert(make_pair(name, vector<UnitConvert>()));
    ValueList objects = value.get_value<ValueList>();
    for (unsigned int i = 0; i < objects.size(); i++) {
        UnitConvert convert;
        convert.set(objects[i].get_value<ValueMap>());
        library_[name].push_back(convert);
    }
}


int Style::score(const MetaDataCollector& data) {
    int bestscore = 0;
    map<string, string> criteria;
    static bool debug;
    static bool first = true;
    if ( first ) {
        first = false;
        debug = (getEnvVariable("MAGICS_STYLES_DEBUG") != "");
    }
 
    for (auto match = criteria_.begin(); match != criteria_.end(); ++match) {
        int score = 0;
        for (auto key = match->begin(); key != match->end(); ++key) {
            auto dkey = data.find(key->first);

            
            if (dkey == data.end()) {
                continue;
            }

            
            
            int tmpscore = 0;
            for (auto value = key->second.begin(); value != key->second.end(); ++value) {
                string whitespaces(" \t\f\v\n\r");
                string clean    = dkey->second;
                std::size_t pos = clean.find_last_not_of(whitespaces);
                if (pos != std::string::npos) {
                    clean = clean.substr(0, pos + 1);
                }
                if (*value == clean) {
                    tmpscore++;
                    criteria.insert(make_pair(key->first, *value));
                    if ( debug) 
                        cout << " Found match " << " --> " <<  key->first << " --> " << *value << " == " << dkey->second << " --> score -> " << score << ", " << criteria.size() << endl;
                    break;
                }
            }

            if (!tmpscore) {
                if ( score && debug ) 
                    cout << "Match not possible" << endl;
                //criteria.clear();
                score = 0;
                break;
            }
            score++;
        }

        if (bestscore < score)
            bestscore = score;
    }

    if (bestscore) {
        if (styles_.empty()) {
            styles_.push_back("default");
        }

        else {
            if (debug) {
                cout << "----   Found style with score : " << bestscore << " Style --> " << styles_.front() << endl;
                for (auto match = criteria.begin(); match != criteria.end(); ++match) {
                    cout << "    " << match->first << " == " << match->second << endl;
                }
                cout  << "---------------------------------------------" << endl;
            }
        }
    }

    return bestscore;
}

void Style::keywords(std::set<string>& keys) {
    for (auto match = criteria_.begin(); match != criteria_.end(); ++match) {
        for (auto key = match->begin(); key != match->end(); ++key) {
            keys.insert(key->first);
        }
    }
}

void StyleLibrary::findStyle(const string& name, MagDef& visdef) {
    allStyles_.find(name, visdef);
}

void StyleLibrary::getCriteria(std::set<string>& keys) {
    for (vector<Style>::iterator style = library_.begin(); style != library_.end(); ++style) {
        style->keywords(keys);
    }
}


bool StyleLibrary::findStyle(const MetaDataCollector& data, MagDef& visdef, StyleEntry& info) {
    int score = 0;
    Style beststyle;

    for (vector<Style>::iterator style = library_.begin(); style != library_.end(); ++style) {
        int s = style->score(data);
        if (s > score) {
            score     = s;
            beststyle = *style;
        }
    }
    if (score) {
        info.set(beststyle.styles_.front(), beststyle.styles_);
        allStyles_.find(info.default_, visdef);
        if (visdef.find("prefered_units") == visdef.end())
            if (beststyle.preferedUnits_.size())
                visdef.insert(make_pair("prefered_units", beststyle.preferedUnits_));
        return true;
    }

    vector<string> empty;
    empty.push_back("default");
    info.set("default", empty);

    allStyles_.find(info.default_, visdef);
    return true;
}

string StyleLibrary::getAttribute(const string& style, const string& param, const string& defval) {
    MagDef visdef;
    allStyles_.find(style, visdef);
    auto value = visdef.find(param);

    return (value != visdef.end()) ? value->second : defval;
}

bool StyleLibrary::findScaling(const MetaDataCollector& data, MagDef& scaling) {
    return false;
}

void NetcdfGuess::init() {
    ostringstream name;
    name << name_ << ".json";
    string library = buildConfigPath(name.str());
    MagLog::debug() << "Opening " << library << endl;
    MagConfigHandler(library, *this);
}

void NetcdfGuess::callback(const string& name, const Value& value) {
    guess_.insert(make_pair(name, map<string, vector<string> >()));
    if (value.isMap()) {
        ValueMap object = value.get_value<ValueMap>();
        for (auto entry = object.begin(); entry != object.end(); ++entry) {
            guess_[name].insert(make_pair(entry->first, vector<string>()));
            ValueList values = (entry->second).get_value<ValueList>();
            for (unsigned int i = 0; i < values.size(); i++) {
                guess_[name][entry->first].push_back(convert(values[i]));
            }
        }
    }
}

void DimensionGuess::init() {

    try {
        Value value = MagParser::decodeString(definitions_);

        ValueMap object = value.get_value<ValueMap>();

        for (auto entry = object.begin(); entry != object.end(); ++entry) {
            // cout << entry->first << endl;
            ValueMap o = entry->second.get_value<ValueMap>();
            map<string, string> def;
            for (auto e = o.begin(); e != o.end(); ++e) {
                // cout << e->name_ << "-->" << MagConfig::convert(e->value_) << endl;
                def.insert(make_pair(e->first, MagConfig::convert(e->second)));
            }
            data_.insert(make_pair(entry->first, def));
        }
    }
    catch (std::exception& e) {
        if (MagicsGlobal::strict()) {
            throw;
        }
        MagLog::error() << "JSON error in " << definitions_ << ": " << e.what() << endl;
    }
}


void MagDefLibrary::init(const string& name) {
    string library = buildConfigPath(name);
    MagLog::dev() << "opening -->" << library << endl;
    MagConfigHandler(library, *this);
}

void MagDefLibrary::init(const string& path, const string& name) {
    string library = path + "/" + name;
    MagLog::dev() << "opening -->" << library << endl;
    MagConfigHandler(library, *this);
}

void MagDef::values(const Value& value) {
    ValueMap object = value.get_value<ValueMap>();
}

void MagDef::set(const ValueMap& object) {
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        insert(make_pair(entry->first, MagConfig::convert(entry->second)));
    }
}

void MagDefLibrary::callback(const string& name, const Value& value) {
    MagDef def;
    def.name_                  = name;
    ValueMap object = value.get_value<ValueMap>();
    def.set(object);

    library_.insert(make_pair(name, def));
}
