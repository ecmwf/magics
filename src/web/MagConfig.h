/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef MagConfig_H
#define MagConfig_H

#include "Layer.h"
#include "MagLog.h"
#include "magics.h"


#include <limits>

namespace magics {

class Value;
class ValueMap;
class ValueList;


class MagConfig {
public:
    MagConfig();
    ~MagConfig();

    static string convert(const Value& value);

    virtual void callback(const string&, const Value&) = 0;
    virtual void callback(const ValueList&) {}
};


class MagConfigHandler

{
public:
    MagConfigHandler(const string& config, MagConfig& object);
    virtual ~MagConfigHandler();

    void dig(const Value&);
    void ignore(const Value&) {}

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;


private:
    //! Copy constructor - No copy allowed
    MagConfigHandler(const MagConfigHandler&);
    //! Overloaded << operator to copy - No copy allowed
    MagConfigHandler& operator=(const MagConfigHandler&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const MagConfigHandler& p) {
        p.print(s);
        return s;
    }
};

struct MagDef : map<string, string> {
    void values(const Value&);


    string name_;
    void set(const ValueMap&);
    friend ostream& operator<<(ostream& s, const MagDef& p) {
        s << "[" << endl;
        for (auto def = p.begin(); def != p.end(); ++def)
            s << "   " << def->first << " = " << def->second << "," << endl;
        s << "]" << endl;
        return s;
    }
};

class StyleEntry;

class MagDefLibrary : public MagConfig {
public:
    MagDefLibrary() {}
    MagDefLibrary(const string& name) : name_(name) { init("/styles/" + name + ".json"); }
    MagDefLibrary(const string& theme, const string& name) : name_(name) {
        init("/styles/" + theme + "/" + name + ".json");
    }
    ~MagDefLibrary() {}

    void callback(const string& name, const Value& value);

    void init(const string&);
    void init(const string&, const string&);

    map<string, MagDef> library_;

    bool find(const string& name, MagDef& definition) {
        // cout << "Looking for  " << name << endl;
        map<string, MagDef>::iterator def = library_.find(name);

        if (def != library_.end()) {
            definition = def->second;
            // cout << "FOUND " << name << endl;
            // cout << def->second << endl;
            return true;
        }
        MagLog::warning() << " Can not find the preset " << name << " for " << name_ << endl;
        return false;
    }
    string name_;
};


struct Style {
    typedef map<string, vector<string> > Match;

    typedef void (Style::*SetMethod)(const Value&);
    map<string, SetMethod> methods_;

    void criteria(const Value&);
    void style(const Value&);
    void units(const Value&);
    void styles(const Value&);
    void name(const Value&);
    void match(const Value&);
    void ignore(const Value&) {}

    vector<Match> criteria_;
    string preferedUnits_;

    vector<string> styles_;


    void set(const ValueMap&);
    void set(ValueMap&, Style::Match&);

    int score(const MetaDataCollector& data);

    void keywords(std::set<string>&);
};

class StyleLibrary : public MagConfig {
public:
    StyleLibrary() {}
    StyleLibrary(const string& path) : path_(path) { init(); }
    ~StyleLibrary() {}

    void callback(const string& name, const Value& value);
    void callback(const ValueList& values);

    void init();

    vector<Style> library_;

    string theme_;
    string family_;
    string path_;
    string current_;
    MagDefLibrary allStyles_;

    bool findStyle(const MetaDataCollector& data, MagDef& visdef, StyleEntry&);
    void findStyle(const string&, MagDef& visdef);
    bool findScaling(const MetaDataCollector& data, MagDef& visdef);

    string getAttribute(const string&, const string&, const string&);

    bool findStyle(const string& name, MagDef& visdef, StyleEntry& info) {
        MetaDataCollector criteria;
        criteria["name"] = name;
        return findStyle(criteria, visdef, info);
    }

    void getCriteria(std::set<string>&);
};


struct Palette {
    typedef vector<string> Definition;
    typedef void (Palette::*SetMethod)(const Value&);

    map<string, SetMethod> methods_;

    void values(const Value&);
    void tags(const Value&);


    Definition colours_;
    Definition tags_;
    string name_;
    void set(const ValueMap&);
};


class PaletteLibrary : public MagConfig {
public:
    PaletteLibrary() { init(); }
    PaletteLibrary(const string& family) { init(); }
    ~PaletteLibrary() {}

    void callback(const string& name, const Value& value);

    void init();

    map<string, Palette> library_;

    bool find(const string& name, Palette::Definition& colours) {
        map<string, Palette>::iterator palette = library_.find(name);

        if (palette != library_.end()) {
            colours = palette->second.colours_;
            return true;
        }
        return false;
    }
};

class NetcdfGuess : public MagConfig {
public:
    NetcdfGuess(const string& name) : name_(name) { init(); }
    NetcdfGuess() : name_("netcdf-convention") { init(); }
    ~NetcdfGuess() {}

    void callback(const string& name, const Value& value);
    void init();

    string name_;

    map<string, map<string, vector<string> > > guess_;
    const map<string, string>& get(const string& name) const;
};

class DimensionGuess : public MagConfig {
public:
    DimensionGuess(const string& def) : definitions_(def) { init(); }

    ~DimensionGuess() {}

    void callback(const string& name, const Value& value) {}
    void init();

    string definitions_;

    map<string, map<string, string> > data_;
    const map<string, string>& get(const string& name) const;
};


struct UnitConvert {
    typedef void (UnitConvert::*SetMethod)(const Value&);

    map<string, SetMethod> methods_;

    void from(const Value&);
    void to(const Value&);
    void scaling(const Value&);
    void offset(const Value&);

    void set(const ValueMap&);

    double scaling();
    double offset();

    string from_;
    string to_;
    double scaling_;
    double offset_;
};


class UnitsLibrary : public MagConfig {
public:
    UnitsLibrary() { init(); }
    UnitsLibrary(const string& family) { init(); }
    ~UnitsLibrary() {}

    void callback(const string& name, const Value& value);

    void init();

    map<string, vector<UnitConvert> > library_;

    void find(const string& need, const string& from, double& scaling, double& offset) {
        scaling = 1;
        offset  = 0;
        if (need == from)
            return;

        map<string, vector<UnitConvert> >::iterator converters = library_.find(need);
        if (converters == library_.end())
            return;


        for (auto converter = converters->second.begin(); converter != converters->second.end(); ++converter) {
            // cout << "UNIT CONVERTER" << converter->from_ << ": " << converter->from_.size() << " == " <<  from << ":
            // " << from.size() << endl;
            if (converter->from_ == from) {
                scaling = converter->scaling_;
                offset  = converter->offset_;
                return;
            }
        }
    }
};


}  // namespace magics
#endif
