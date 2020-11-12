/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ContourLibrary.h
    \brief Definition of the Template class ContourLibrary.

    Magics Team - ECMWF 2010

    Started: Fri 16-Jul-2010

    Changes:

*/


#include "CliMetLabLibrary.h"
#include "Data.h"
#include "MagParser.h"
#include "MetaData.h"


#if ((defined(_MSVC_LANG) && _MSVC_LANG >= 201703L) || __cplusplus >= 201703L)
#include <filesystem>
namespace fs = std::filesystem;
#else
#define _SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;
#endif


namespace magics {

class Library {
    size_t counter_ = 0;
    std::string path_;

    std::set<std::string> keys_;
    std::map<std::string, ValueMap> styles_;
    std::vector<ValueMap> rules_;

    void process(const std::string& path, const ValueMap& entry, int n);

public:
    Library(const std::string& path);
    StyleEntry* getStyle(Data& data, MagDef& visdef) const;

    void reset();
    void build();
};


Library::Library(const std::string& path) : path_(path) {
    build();
}

void Library::build() {
    size_t n = 0;
    std::vector<std::string> entries;

    if (path_ == "default") {
        path_ = buildConfigPath("styles", "climetlab");
    }
    MagLog::dev() << "SCANNING " << path_ << std::endl;
    for (auto& p : fs::recursive_directory_iterator(path_)) {
        std::string ext  = p.path().extension().string();
        std::string full = p.path().string();

        if (ext == ".yaml" || ext == ".json") {
            entries.push_back(full);
        }
    }
    MagLog::dev() << "DONE " << path_ << std::endl;
    n++;

    std::sort(entries.begin(), entries.end());

    for (const auto& path : entries) {
        try {
            Value m = MagParser::decodeFile(path);
            if (m.isList()) {
                ValueList l = m;
                int n       = 0;
                for (auto& v : l) {
                    process(path, v, n++);
                }
            }
            else {
                process(path, m, 0);
            }
        }
        catch (std::exception& e) {
            MagLog::error() << "Error processing " << path << ": " << e.what() << ", ignored." << std::endl;
        }
    }

    // Collected which values are needed by the rukes

    for (ValueMap rule : rules_) {
        ValueList matches = rule["match"];
        for (ValueMap match : matches) {
            for (auto j = match.begin(); j != match.end(); ++j) {
                keys_.insert((*j).first);
            }
        }
    }

}  // namespace magics

void Library::process(const std::string& path, const ValueMap& entry, int n) {
    bool style = (entry.find("magics") != entry.end());
    bool rule  = (entry.find("match") != entry.end());

    if (rule && style) {
        std::ostringstream oss;
        oss << fs::path(path).stem() << "/" << (counter_++) << "/" << n;
        std::string name = oss.str();

        ASSERT(entry.find("styles") == entry.end());
        ValueList styles;
        styles.push_back(name);

        ValueMap e  = entry;
        e["styles"] = styles;

        styles_[name] = e;
        rules_.push_back(e);


        return;
    }

    if (rule) {
        rules_.push_back(entry);
        return;
    }

    if (style) {
        std::string name = fs::path(path).stem().string();
        styles_[name]    = entry;
        return;
    }
}

StyleEntry* Library::getStyle(Data& data, MagDef& visdef) const {
    MetaDataCollector collector;
    for (const auto& criteria : keys_) {
        collector[criteria] = "";
        MetaDataAttribute attribute;
        attribute.setSource(MetaDataAttribute::GribApiSource);
        collector.setAttribute(criteria, attribute);
    }


    data.visit(collector);


    // Get values from the grib or necdf

    MagLog::dev() << "=== DATA" << std::endl;
    for (auto j = collector.begin(); j != collector.end(); ++j) {
        if ((*j).second.size()) {
            MagLog::dev() << "--- " << (*j).first << " = " << (*j).second << std::endl;
        }
    }

    int score = -1;
    Value best;

    for (ValueMap rule : rules_) {
        ValueList matches = rule["match"];
        for (ValueMap match : matches) {
            int same = 0;
            for (auto j = match.begin(); j != match.end(); ++j) {
                std::string key = (*j).first;

                if ((*j).second.isList()) {
                    ValueList vals = (*j).second;
                    for (std::string val : vals) {
                        if (collector[key] == val) {
                            same++;
                            break;
                        }
                    }
                }
                else {
                    std::string val = (*j).second;

                    if (collector[key] == val) {
                        same++;
                    }
                }
            }

            if (same == match.size()) {
                if (same > score) {
                    score = same;
                    best  = rule;
                }
            }
        }
    }

    if (score == -1) {
        // FIXME
        // {
        //     ofstream out("style");
        //     out << "default" << std::endl;
        // }
        return nullptr;
    }

    MagLog::dev() << best << std::endl;

    std::vector<std::string> styles;

    if (best.contains("style")) {
        styles.push_back(std::string(best["style"]));
    }
    else {
        ValueList v = best["styles"];
        for (auto s : v) {
            styles.push_back(std::string(s));
        }
    }

    std::string style_name = styles[0];

    // {
    //     ofstream out("style");
    //     out << style_name << std::endl;
    // }

    auto j = styles_.find(style_name);
    if (j == styles_.end()) {
        MagLog::error() << "Error style: " << style_name << " not found." << std::endl;
        return nullptr;
    }

    ValueMap style   = (*j).second;
    ValueMap contour = style["magics"]["mcont"];

    MagDef result;
    for (auto j = contour.begin(); j != contour.end(); ++j) {
        std::string key  = (*j).first;
        const Value& val = (*j).second;
        if (val.isList()) {
            std::ostringstream oss;
            const char* sep  = "";
            ValueList values = val;
            for (auto& v : values) {
                if (val.isBool()) {
                    oss << sep << (bool(v) ? "on" : "off");
                }
                else {
                    oss << sep << v;
                }
                sep = "/";
            }
            result[(*j).first] = oss.str();
            continue;
        }

        if (val.isBool()) {
            result[(*j).first] = bool(val) ? "on" : "off";
        }
        else {
            result[(*j).first] = std::string(val);
        }
    }

    // If not found, seach by units


    visdef = result;

    MagLog::dev() << "=== VISDEF" << std::endl;
    for (auto j = visdef.begin(); j != visdef.end(); ++j) {
        MagLog::dev() << "--- " << (*j).first << " = " << (*j).second << std::endl;
    }

    StyleEntry* s = new StyleEntry();
    styles.push_back(style_name);
    s->set(style_name, styles);

    // TODO: fill the entry
    return s;
}

void Library::reset() {
    // Placeholder to reload if needed
    if (true) {
        keys_.clear();
        styles_.clear();
        rules_.clear();
        build();
    }
}


static std::map<std::string, Library*> cache_;
static std::vector<const Library*> libraries_;

static void init(const std::string& library_path) {
    Tokenizer tokenizer(":");
    vector<string> paths;
    tokenizer(library_path, paths);

    libraries_.clear();
    for (const auto& path : paths) {
        auto j = cache_.find(path);
        if (j == cache_.end()) {
            cache_[path] = new Library(path);
            j            = cache_.find(path);
        }
        else {
            (*j).second->reset();
        }
        libraries_.push_back((*j).second);
    }
}

CliMetLabLibrary::CliMetLabLibrary() {}

CliMetLabLibrary::~CliMetLabLibrary() {}

StyleEntry* CliMetLabLibrary::getStyle(Data& data, const std::string& library_path, MagDef& visdef) {
    if (library_path.empty()) {
        init("default");
    }
    else {
        init(library_path);
    }

    for (auto library : libraries_) {
        StyleEntry* e = library->getStyle(data, visdef);
        if (e != nullptr) {
            return e;
        }
    }

    return nullptr;
}

void CliMetLabLibrary::print(ostream& out) const {
    out << "CliMetLabLibrary[]";
}

static SimpleObjectMaker<CliMetLabLibrary, ContourLibrary> climetlab("climetlab");


}  // namespace magics
