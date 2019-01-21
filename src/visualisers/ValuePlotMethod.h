/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ValuePlotMethod.h
    \brief Definition of the Template class ValuePlotMethod.

    Magics Team - ECMWF 2004

    Started: Thu 26-Aug-2004

    Changes:

*/

#ifndef ValuePlotMethod_H
#define ValuePlotMethod_H

#include "magics.h"

#include "MagicsFormat.h"
#include "MatrixHandler.h"
#include "PointsHandler.h"
#include "Text.h"
#include "Transformation.h"
#include "ValuePlotMethodAttributes.h"

namespace magics {


class ValuePlotMethod : public ValuePlotMethodAttributes, public vector<BasicGraphicsObject*> {
public:
    ValuePlotMethod() {}
    virtual ~ValuePlotMethod() {}
    virtual void set(const map<string, string>& map) { ValuePlotMethodAttributes::set(map); }
    virtual void set(const XmlNode& node) { ValuePlotMethodAttributes::set(node); }

    virtual ValuePlotMethod* clone() const {
        ValuePlotMethod* object = new ValuePlotMethod();
        object->copy(*this);
        return object;
    }
    virtual void operator()(MatrixHandler& data, const Transformation& transformation) {
        reset();
        int rows    = data.rows();
        int columns = data.columns();
        for (int j = 0; j < rows; j += lat_frequency_) {
            for (int i = 0; i < columns; i += lon_frequency_) {
                double val = data(j, i);
                if (min_ <= val && val <= max_ && val != data.missing()) {
                    UserPoint point(data.column(j, i), data.row(j, i), data(j, i));
                    PaperPoint xy = transformation(point);
                    if (transformation.in(xy))
                        add(xy);
                }
            }
        }
    }
    virtual void operator()(PointsHandler& data, const Transformation& transformation) {
        reset();
        ThinningPointsHandler thinned(data, lon_frequency_, lat_frequency_);
        thinned.setToFirst();

        while (thinned.more()) {
            UserPoint point = thinned.current();
            double val      = point.value();
            if (min_ <= val && val <= max_) {
                PaperPoint xy = transformation(point);
                if (transformation.in(xy))
                    add(xy);
            }
            thinned.advance();
        }
    }


protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream& out) const {
        out << "ValuePlotMethod[";
        ValuePlotMethodAttributes::print(out);
        out << "]";
    }
    virtual void reset() {}  // For Metview, get ready for a second frame.
    virtual void add(const PaperPoint& xy) {
        static map<string, VerticalAlign> alignhandlers;
        if (alignhandlers.empty()) {
            alignhandlers["normal"] = MNORMAL;
            alignhandlers["top"]    = MTOP;
            alignhandlers["cap"]    = MCAP;
            alignhandlers["half"]   = MHALF;
            alignhandlers["base"]   = MBASE;
            alignhandlers["bottom"] = MBOTTOM;
        }
        ostringstream nice;
        nice << MagicsFormat(format_, xy.value());
        Text* text = new Text();
        text->addText(nice.str(), *colour_, height_);
        text->setJustification(justification_);
        map<string, VerticalAlign>::iterator pos = alignhandlers.find(lowerCase(vertical_align_));
        VerticalAlign align                      = (pos != alignhandlers.end()) ? pos->second : MBASE;
        text->setVerticalAlign(align);
        text->push_back(xy);
        push_back(text);
    }

    string label(double val) {
        ostringstream nice;
        nice << MagicsFormat(format_, val);
        return nice.str();
    }

private:
    //! Copy constructor - No copy allowed
    ValuePlotMethod(const ValuePlotMethod&);
    //! Overloaded << operator to copy - No copy allowed
    ValuePlotMethod& operator=(const ValuePlotMethod&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const ValuePlotMethod& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, ValuePlotMethod> {
public:
    ValuePlotMethod* operator()(const string& val) { return SimpleObjectMaker<ValuePlotMethod>::create(val); }
    ValuePlotMethod* magics(const string& param) {
        ValuePlotMethod* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics


#endif
