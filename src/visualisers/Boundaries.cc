/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Boundaries.cc
    \brief Implementation of the Template class BoundariesBase.

    Magics Team - ECMWF 2006

    Started: Tue 29-Aug-2006

    Changes:
         2010 JUL change from mapgen to shp

*/


#include "Boundaries.h"
#include "Polyline.h"
#include "ShapeDecoder.h"
#include "UserPoint.h"

using namespace magics;

Boundaries::Boundaries() {}

Boundaries::~Boundaries() {}


/*!
 Class information are given to the output-stream.
*/
void Boundaries::print(ostream& out) const {
    out << "Boundaries[";
    BoundariesAttributes::print(out);
    out << "]";
}


void Boundaries::operator()(const map<string, string>& setting, BasicGraphicsObjectContainer& task) {
    ShapeDecoder boundaries, disputed;
    boundaries.needHoles(true);
    disputed.needHoles(true);

    const string political_boundaries      = setting.find("boundaries")->second;
    const string administrative_boundaries = setting.find("administrative_boundaries")->second;


    string file = buildConfigPath(political_boundaries);

    boundaries.setPath(file);
    vector<string> treaty;
    treaty.push_back("International boundary");
    treaty.push_back("Country_Boundary");
    treaty.push_back("Indefinite (please verify)");

    boundaries.decode(task.transformation(), "featurecla", treaty);
    const Transformation& transformation = task.transformation();

    static map<string, vector<string> > predefined_list;


    if (predefined_list.empty()) {
        predefined_list["south_america"] = {"ARG", "BRA", "BOL", "HL",  "COL", "GUY",
                                            "PRY", "PER", "SUR", "URY", "VEN", "ECU"};
        predefined_list["oceania"]       = {"AUS", "CCK", "CXR", "HMD", "NFK", "NCL", "NZL", "PNG", "SLB", "VUT"};
        predefined_list["asia"]          = {"AFG", "BGD", "CHN", "TLS", "IND", "IDN", "IRN", "IRQ", "ISR", "JPN",
                                   "KAZ", "PRK", "KOR", "KGZ", "LAO", "MYS", "MNG", "MMR", "NPL", "PAK",
                                   "SAU", "SYR", "TJK", "TUR", "TKM", "UZB", "YEM", "ZMB", "ZWE"};
        predefined_list["north_america"] = {"CAN", "CUB", "MEX", "USA"};
        predefined_list["africa"]        = {"DZA", "AGO", "CAF", "TCD", "COG", "COD", "BWA", "CMR", "EGY", "ERI", "ETH",
                                     "GAB", "GMB", "CIV", "GHA", "GIN", "KEN", "LBY", "MLI", "MRT", "MDG", "MAR",
                                     "MOZ", "NAM", "NER", "NGA", "RWA", "SEN", "SOM", "ZAF", "SDN", "TZA", "TGO"};
        predefined_list["europe"]        = {"AUT", "BLR", "RUS", "CZE", "FRO", "FIN", "ALA", "FRA", "DEU", "GRC",
                                     "SJM", "ITA", "POL", "ROU", "SRB", "ESP", "UKR", "GBR", "GIB"};
    }


    vector<string> admin;

    for (vector<string>::const_iterator a = administrative_list_.begin(); a != administrative_list_.end(); ++a) {
        auto expand = predefined_list.find(lowerCase(*a));
        if (expand != predefined_list.end()) {
            for (auto e = expand->second.begin(); e != expand->second.end(); ++e)
                admin.push_back(*e);
        }
        else
            admin.push_back(*a);
    }


    if (admistrative_) {
        ShapeDecoder admistrative;
        admistrative.needHoles(true);
        file = buildConfigPath(administrative_boundaries);

        admistrative.setPath(file);
        admistrative.decode(task.transformation(), "adm0_a3", admin);
        const Transformation& transformation = task.transformation();

        for (ShapeDecoder::const_iterator boundary = admistrative.begin(); boundary != admistrative.end(); ++boundary) {
            Polyline poly;
            poly.setColour(administrative_colour_->automatic() ? *colour_ : *administrative_colour_);
            poly.setThickness(administrative_thickness_);
            poly.setLineStyle(administrative_style_);

            (**boundary).setToFirst();
            while ((**boundary).more()) {
                poly.push_back(transformation((**boundary).current()));
                (**boundary).advance();
            }
            transformation(poly, task);
        }
    }

    for (ShapeDecoder::const_iterator boundary = boundaries.begin(); boundary != boundaries.end(); ++boundary) {
        Polyline poly;
        poly.setColour(*colour_);
        poly.setThickness(thickness_);
        poly.setLineStyle(style_);

        (**boundary).setToFirst();
        while ((**boundary).more()) {
            poly.push_back(transformation((**boundary).current()));
            (**boundary).advance();
        }
        transformation(poly, task);
    }

    if (disputed_) {
        ShapeDecoder disputed;
        disputed.needHoles(true);

        vector<string> dis;
        dis.push_back("Disputed");
        dis.push_back("Line of control (please verify)");
        file = buildConfigPath(political_boundaries);

        disputed.setPath(file);
        disputed.decode(task.transformation(), "featurecla", dis);
        const Transformation& transformation = task.transformation();

        for (ShapeDecoder::const_iterator boundary = disputed.begin(); boundary != disputed.end(); ++boundary) {
            Polyline poly;
            poly.setColour(disputed_colour_->automatic() ? *colour_ : *disputed_colour_);
            poly.setThickness(disputed_thickness_);
            poly.setLineStyle(disputed_style_);


            (**boundary).setToFirst();
            while ((**boundary).more()) {
                poly.push_back(transformation((**boundary).current()));
                (**boundary).advance();
            }
            transformation(poly, task);
        }
    }
}


NoBoundaries::NoBoundaries() {}

NoBoundaries::~NoBoundaries() {}

void NoBoundaries::print(ostream& out) const {
    out << "NoBoundaries[";
    out << "]";
}
