/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file SpotDecoder.h
    \brief Implementation of the Template class SpotDecoder.

    Magics Team - ECMWF 2005

    Started: Mon 19-Sep-2005

    Changes:

*/


#include "EpsXmlInput.h"
#include "CustomisedPoint.h"
#include "MagDateTime.h"
#include "IntervalMap.h"
#include "LegendVisitor.h"
#include "MetaData.h"
#include "TextVisitor.h"
#include "XmlReader.h"

#include <limits>
#include <locale>
#include "Factory.h"

using namespace magics;

namespace magics {

void EpsXmlInput::visit(Transformation& transformation) {
    decode();

    transformation.setDataMinMaxX((minstep_ - dateOffset_) * 3600, (maxstep_ + dateOffset_) * 3600, base_);


    dateOffset_ = minstep_ - dateOffset_;

    transformation.setMinMaxY(miny_, maxy_);
}

void EpsXmlInput::decode() {
    if (!points_.empty())
        return;
    minstep_ = std::numeric_limits<double>::max();
    maxstep_ = std::numeric_limits<double>::min();
    miny_    = std::numeric_limits<double>::max();
    ;
    maxy_ = std::numeric_limits<double>::min();
    ;

    XmlReader parser(true);
    XmlTree tree;

    try {
        parser.interpret(path_, &tree);
        tree.visit(*this);
    }
    catch (MagicsException& e) {
        MagLog::debug() << e.what() << endl;
    }
}

void EpsXmlInput::customisedPoints(const std::set<string>&, CustomisedPointsList& out) {
    MagLog::dev() << "EpsXmlInput::customisedPoints-->need to be implemented" << std::endl;
    MagLog::dev() << *this << endl;
    decode();

    for (vector<CustomisedPoint*>::iterator point = points_.begin(); point != points_.end(); ++point) {
        (**point)["step"] -= (dateOffset_ * 3600);
        out.push_back(*point);
    }
}


void EpsXmlInput::visit(TextVisitor& title) {
    if (!long_title_ && !short_title_)
        return;
    decode();
    if (long_title_) {
        ostringstream out;
        tm convert = base_;
        locale loc("");
        out.imbue(loc);
        const std::time_put<char>& tfac = use_facet<time_put<char> >(loc);
        string format                   = "Forecast %A %e %B %Y %H UTC";
        tfac.put(out, out, ' ', &convert, format.c_str(), format.c_str() + format.length());

        ostringstream line;
        UserPoint position(longitude_, latitude_);
        line << station_ << "(" << position.asLatitude() << ", " << position.asLongitude() << ")" << endl;
        title.addAutomaticTitle(title_);
        title.addAutomaticTitle(line.str());
        title.addAutomaticTitle(out.str());
    }
    if (short_title_) {
        title.addAutomaticTitle("");
        title.addAutomaticTitle(parameter_);
    }
}

void EpsXmlInput::visit(MetaDataVisitor&) {}

void EpsXmlInput::print(ostream& out) const {
    out << "EpsXmlInput[";
    EpsXmlInputAttributes::print(out);
    out << "]";
}

EpsXmlInput::EpsXmlInput() : dateOffset_(6) {}

EpsXmlInput::~EpsXmlInput() {}

#define value(s) (tonumber(node.getAttribute(s)) * scaling) + offset

void EpsXmlInput::visit(const XmlNode& node) {
    MagLog::dev() << node.name() << endl;
    if (node.name() == "forecast") {
        MagDate date(node.getAttribute("date"));
        MagTime time(node.getAttribute("time"));
        station_   = node.getAttribute("station");
        title_     = node.getAttribute("title");
        latitude_  = tonumber(node.getAttribute("latitude"));
        longitude_ = tonumber(node.getAttribute("longitude"));
        base_      = DateTime(date, time);
        node.visit(*this);
    }

    static double scaling;
    static double offset;

    if (node.name() == "param" && node.getAttribute("name") == param_) {
        string s   = node.getAttribute("scaling");
        parameter_ = node.getAttribute("title");
        scaling    = (s.empty()) ? 1 : tonumber(s);
        string o   = node.getAttribute("offset");
        offset     = (o.empty()) ? 0 : tonumber(o);

        node.visit(*this);
    }

    if (node.name() == "step") {
        MagLog::dev() << node.getAttribute("value") << endl;
        double step = tonumber(node.getAttribute("value"));
        if (step < minstep_)
            minstep_ = step;
        if (step > maxstep_)
            maxstep_ = step;
        double min = value("min");
        double max = value("max");
        if (min < miny_)
            miny_ = min;
        if (max > maxy_)
            maxy_ = max;
        CustomisedPoint* point = new CustomisedPoint();
        point->longitude(step * 3600);
        // point->insert(make_pair("shift", dateOffset_*3600));
        point->insert(make_pair("shift", 0));
        point->insert(make_pair("step", step * 3600));
        point->insert(make_pair("width", 1.5 * 3600));

        point->insert(make_pair("year", base_.date().year()));
        point->insert(make_pair("month", base_.date().month()));
        point->insert(make_pair("day", base_.date().day()));
        point->insert(make_pair("hours", base_.time().hours()));
        point->insert(make_pair("minutes", base_.time().minutes()));
        point->insert(make_pair("seconds", base_.time().seconds()));
        point->insert(make_pair("min", value("min")));
        point->insert(make_pair("max", value("max")));
        point->insert(make_pair("ten", value("ten")));
        point->insert(make_pair("twenty_five", value("twentyfive")));
        point->insert(make_pair("median", value("median")));
        point->insert(make_pair("seventy_five", value("seventyfive")));
        point->insert(make_pair("ninety", value("ninety")));
        if (!node.getAttribute("forecast").empty())
            point->insert(make_pair("hres", value("forecast")));
        if (!node.getAttribute("control").empty())
            point->insert(make_pair("control", value("control")));

        points_.push_back(point);
    }
}
}  // namespace magics
