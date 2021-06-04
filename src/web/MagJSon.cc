/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MagJSon.h"


using namespace magics;
#include "XmlMagics.h"
#include "XmlTree.h"

#include "MagParser.h"
#include "Timer.h"
#include "Value.h"

MagJSon::MagJSon() {
    patchs_["drivers"]    = &MagJSon::drivers;
    patchs_["definition"] = &MagJSon::definitions;
}

void MagJSon::execute(const string& magml, const map<string, string>& params) {
    TempFile file;

    prepare(magml, params, file);
    parse(file.name());
}

void MagJSon::parse(const string& file) {
    Value value = MagParser::decodeFile(file);
    magics(value);
}

void MagJSon::interpret(const string& def) {
    MagLog::dev() << "interpret-->" << def << endl;
    Value value = MagParser::decodeString(def);

    ValueMap object = value.get_value<ValueMap>();
    // buils the Magics XmlNode!
    build(*tree_.root(), "magics", object);
}

void ParamJSon::magics(const Value& value) {

    ValueMap object = value.get_value<ValueMap>();
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        this->insert(make_pair(entry->first, string(entry->second)));
    }
}

ParamJSon::ParamJSon(const string& param) {
    if (param.empty())
        return;
    Value value = MagParser::decodeString(param);
    magics(value);
}


void MagJSon::drivers(XmlNode& parent, const Value& value) {

    XmlNode* drivers = new XmlNode("drivers");
    parent.push_back(drivers);
    ValueList all = value.get_value<ValueList>();

    for (auto entry = all.begin(); entry != all.end(); ++entry) {
        ASSERT(entry->isMap());
        ValueMap driver = entry->get_value<ValueMap>();
        map<string, string> attributes;
        for (auto elt = driver.begin(); elt != driver.end(); ++elt) {
            attributes.insert(make_pair(elt->first, elt->second.get_value<string>()));
        }
        map<string, string>::iterator format = attributes.find("format");
        ASSERT(format != attributes.end());
        drivers->push_back(tree_.newNode(format->second, attributes));
    }
}


void MagJSon::definitions(XmlNode& parent, const Value& value) {

    XmlNode* definitions = new XmlNode("definition");
    tree_.definition(definitions);

    ValueList all = value.get_value<ValueList>();
    for (auto entry = all.begin(); entry != all.end(); ++entry) {
        ValueMap def = entry->get_value<ValueMap>();
        map<string, string> attributes;
        for (auto elt = def.begin(); elt != def.end(); ++elt) {
            ASSERT(elt->second.isString());
            attributes.insert(make_pair(elt->first, elt->second.get_value<string>()));
        }
        map<string, string>::iterator type = attributes.find("class");
        ASSERT(type != attributes.end());
        definitions->push_back(tree_.newNode(type->second, attributes));
    }
}


void MagJSon::build(XmlNode& parent, const string& name, ValueMap& object) {
    map<string, string> attributes;

    for (auto entry = object.begin(); entry != object.end(); ++entry) {

        if (entry->second.isBool()) {
            string value = entry->second.get_value<bool>() ? "on" : "off";
            attributes.insert(make_pair(entry->first, value));
        }
        else if ( !entry->second.isList() && !entry->second.isMap() && !entry->second.isOrderedMap() ) {
            attributes.insert(make_pair(entry->first, string(entry->second)));
        }
    }
    
    
    XmlNode* node = tree_.newNode(name, attributes);
    parent.push_back(node);
    
    for (auto entry = object.begin(); entry != object.end(); ++entry) {
        // We can apply a patch ..
        map<string, Patch>::iterator patch = patchs_.find(entry->first);
        if (patch != patchs_.end()) {
            ((this->*patch->second)(*node, entry->second));
            continue;
        }

        if (entry->second.isMap() ) {
            ValueMap object = entry->second.get_value<ValueMap>();
            build(*node, entry->first, object);
        }
        if (entry->second.isList()) {
            ValueList object = entry->second.get_value<ValueList>();
            for (auto val = object.begin(); val != object.end(); ++val) {
                if (val->isMap()) {
                    ValueMap object = val->get_value<ValueMap>();
                    build(*node, entry->first, object);
                }
            }
        }
    }
}


void MagJSon::magics(const Value& value) {
    ValueMap object = value.get_value<ValueMap>();
    XmlMagics magics;
    // buils the Magics XmlNode!
    build(*tree_.root(), "magics", object);
    Timer timer("total", "execution");
    magics.execute(tree_);
}

bool ParamJSon::hasKey(const string& key) {
    return (find(key) != end());
}

string ParamJSon::get(const string& key, const string& val) {
    const_iterator value = find(key);
    return (value != end()) ? value->second : val;
}
