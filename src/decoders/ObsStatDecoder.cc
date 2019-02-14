/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ObsStatDecoder.cc
    \brief Implementation of the Template class ObsStatDecoder.

    Magics Team - ECMWF 2005

    Started: Mon 17-Oct-2005

    Changes:

*/


#include "ObsStatDecoder.h"
#include "CustomisedPoint.h"
#include "Tokenizer.h"

using namespace magics;


ObsStatDecoder::ObsStatDecoder() {}


ObsStatDecoder::~ObsStatDecoder() {}

/*!
 Class information are given to the output-stream.
*/
void ObsStatDecoder::print(ostream& out) const {
    out << "ObsStatDecoder[";
    out << "]";
}

bool equal(const string& in, const string& key) {
    if (in.length() < key.length())
        return false;
    string read = in.substr(0, key.length());
    return (read == key);
}

void parsedef(const string& in, string& param, vector<string>& values) {
    Tokenizer tokenizer("= ");
    vector<string> tokens;
    tokenizer(in, tokens);

    vector<string>::const_iterator token = tokens.begin();
    param                                = *token;
    ++token;

    while (token != tokens.end()) {
        values.push_back(*token);
        ++token;
    }
}
void parse(const string& in, map<string, int>& values) {
    Tokenizer tokenizer("# ");
    vector<string> tokens;

    tokenizer(in, tokens);
    int i = 0;
    for (vector<string>::const_iterator token = tokens.begin(); token != tokens.end(); ++token) {
        values[*token] = i++;
    }
}
void parse(const string& in, vector<double>& values) {
    Tokenizer tokenizer(" ");

    vector<string> tokens;

    tokenizer(in, tokens);

    for (vector<string>::const_iterator token = tokens.begin(); token != tokens.end(); ++token) {
        values.push_back(atof((*token).c_str()));
    }
}


StatItem::StatItem(const string& name, ifstream& in) : name_(name) {
    char buf[1024];
    while (in.good()) {
        in.getline(buf, 1024);
        if (string(buf).empty())
            continue;
        if (equal(buf, "END STATITEM"))
            break;
        if (equal(buf, "#Pressure")) {
            parse(buf, columns_);
            continue;
        }
        if (equal(buf, "#")) {
            vector<string> defs;
            string name;
            parsedef(buf, name, defs);
            definitions_[name] = defs;

            continue;
        }
        else {
            vector<double> values;
            parse(buf, values);
            rows_.push_back(values);
        }
    }
}


StatDef::StatDef(const string& name, ifstream& in) : name_(name) {
    char buf[1024];
    while (in.good()) {
        in.getline(buf, 1024);
        if (string(buf).empty())
            continue;
        string name, ref;
        vector<string> defs;
        if (equal(buf, "END STATDEF"))
            continue;
        if (equal(buf, "BEGIN STATITEM")) {
            istringstream line(buf);
            string d1, d2;
            line >> d1 >> d2 >> ref;
            data_.push_back(StatItem(ref, in));
        }
        if (equal(buf, "####################")) {
            break;
        }
        else {
            parsedef(buf, name, defs);
            definitions_[name] = defs;
        }
    }
}


void ObsStatDecoder::decode() {
    ifstream in(path_.c_str());
    char buf[1024];

    string statdef = "BEGIN STATDEF";
    // First get version!
    string name;
    in.getline(buf, 1024);
    version_ = buf;
    MagLog::dev() << "version-->" << version_ << endl;
    // look for BEGIN STATDEF
    while (in.good()) {
        in.getline(buf, 1024);

        if (equal(buf, "BEGIN STATDEF")) {
            istringstream line(buf);
            string d1, d2;
            line >> d1 >> d2 >> name;
            MagLog::dev() << buf << " ---> " << name << endl;
            data_.insert(make_pair(name, StatDef(name, in)));
        }
    }
    in.close();

    for (map<string, StatDef>::const_iterator def = data_.begin(); def != data_.end(); ++def) {
        MagLog::dev() << def->first << "------->" << def->second << endl;
    }
}

void StatItem::print(ostream& out) const {
    out << "BEGIN STATITEM " << name_ << "\n";
    for (map<string, vector<string> >::const_iterator def = definitions_.begin(); def != definitions_.end(); ++def) {
        out << def->first << " = ";
        for (vector<string>::const_iterator val = def->second.begin(); val != def->second.end(); ++val)
            out << *val << " ";
        out << "\n";
    }
    for (map<string, int>::const_iterator column = columns_.begin(); column != columns_.end(); ++column) {
        out << column->first << " ";
    }
    out << "\n";
    for (vector<vector<double> >::const_iterator row = rows_.begin(); row != rows_.end(); ++row) {
        for (vector<double>::const_iterator val = row->begin(); val != row->end(); ++val)
            out << *val << " ";
        out << "\n";
    }
}

void StatDef::print(ostream& out) const {
    out << "BEGIN STATDEF" << name_ << "\n";
    for (map<string, vector<string> >::const_iterator def = definitions_.begin(); def != definitions_.end(); ++def) {
        out << def->first << " = ";
        for (vector<string>::const_iterator val = def->second.begin(); val != def->second.end(); ++val)
            out << *val << " ";
        out << "\n";
    }


    out << "END BEGIN " << name_ << "\n";

    for (vector<StatItem>::const_iterator item = data_.begin(); item != data_.end(); ++item)
        out << *item;
    out << "##########################################\n";
}

void ObsStatDecoder::customisedPoints(const std::set<string>&, CustomisedPointsList& list) {
    decode();

    map<string, StatDef>::const_iterator def = data_.find("1");
    if (def == data_.end())
        return;

    const StatDef& stat = def->second;
    int i               = 0;
    for (vector<StatItem>::const_iterator item = stat.data_.begin(); item != stat.data_.end(); ++item) {
        i++;
        int y = 1;
        for (vector<vector<double> >::const_iterator row = (*item).rows_.begin(); row != (*item).rows_.end(); ++row) {
            vector<double>::const_iterator val = (*row).begin();
            CustomisedPoint* point             = new CustomisedPoint();
            (*point)["total"]                  = (*item).columns_.size();
            list.push_back(point);

            for (map<string, int>::const_iterator column = (*item).columns_.begin(); column != (*item).columns_.end();
                 ++column) {
                ostringstream t;
                t << (column->first) << "_" << i;
                string name = t.str();


                MagLog::dev() << name << "--->" << *val << "[" << y << "]" << endl;
                (*point)[name] = *val;
                (*point)["y"]  = y;
                ++val;
            }
            y++;
        }
    }
}


PointsHandler& ObsStatDecoder::points() {
    decode();
    pointsHandlers_.push_back(new PointsHandler(*this));
    return *(pointsHandlers_.back());
}

/*void ObsStatDecoder::visit(TitleBase&)
{
}
*/
