/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file AxisTickLabelType.h
    \brief Definition of the Template class AxisTickLabel.

    Magics Team - ECMWF 2005

    Started: Fri 7-Oct-2005

    Changes:

*/

#ifndef AxisTickLabelType_H
#define AxisTickLabelType_H

#include "Factory.h"
#include "MagTranslator.h"
#include "magics.h"

#include "LabelListLabelTypeAttributes.h"
#include "NumberLabelTypeAttributes.h"

namespace magics {

class AxisTickLabelType {
public:
    AxisTickLabelType() {}
    virtual ~AxisTickLabelType() {}
    virtual void set(const XmlNode&) {}
    virtual void set(const map<string, string>&) {}
    virtual bool accept(const string&) { return false; }
    virtual AxisTickLabelType* clone() const { return new AxisTickLabelType(); }
    virtual string label(const string& label) { return label; }
    virtual void toxml(ostream&) const;

protected:
    //! Method to print string about this class on to a stream of type ostream (virtual).
    virtual void print(ostream&) const;

private:
    //! Copy constructor - No copy allowed
    AxisTickLabelType(const AxisTickLabelType&);
    //! Overloaded << operator to copy - No copy allowed
    AxisTickLabelType& operator=(const AxisTickLabelType&);

    // -- Friends
    //! Overloaded << operator to call print().
    friend ostream& operator<<(ostream& s, const AxisTickLabelType& p) {
        p.print(s);
        return s;
    }
};

class NumberLabelType : public AxisTickLabelType, public NumberLabelTypeAttributes {
public:
    NumberLabelType() {}
    virtual ~NumberLabelType() {}
    virtual void set(const XmlNode& node) { NumberLabelTypeAttributes::set(node); }
    virtual void set(const map<string, string>& map) { NumberLabelTypeAttributes::set(map); }
    bool accept(const string& node) { return NumberLabelTypeAttributes::accept(node); }
    virtual AxisTickLabelType* clone() const {
        NumberLabelType* label = new NumberLabelType();
        label->copy(*this);
        return label;
    }
    virtual string label(const string& label);
    virtual void print(ostream&) const;
    virtual void toxml(ostream&, int) const;
};

class LabelListLabelType : public AxisTickLabelType, public LabelListLabelTypeAttributes {
public:
    LabelListLabelType();
    virtual ~LabelListLabelType() {}
    virtual void set(const XmlNode& node) {
        LabelListLabelTypeAttributes::set(node);
        current_ = labels_.begin();
    }
    virtual void set(const map<string, string>& map) { LabelListLabelTypeAttributes::set(map); }
    bool accept(const string& node) { return LabelListLabelTypeAttributes::accept(node); }
    virtual AxisTickLabelType* clone() const {
        LabelListLabelType* label = new LabelListLabelType();
        label->copy(*this);
        return label;
    }
    virtual string label(const string& label);
    virtual void print(ostream&) const;
    virtual void toxml(ostream&, int) const;
    map<string, string> cache_;

protected:
    vector<string>::const_iterator current_;
};

template <>
class MagTranslator<string, AxisTickLabelType> {
public:
    AxisTickLabelType* operator()(const string& val) { return SimpleObjectMaker<AxisTickLabelType>::create(val); }

    AxisTickLabelType* magics(const string& param) {
        AxisTickLabelType* object = 0;
        ParameterManager::update(param, object);
        return object;
    }
};

}  // namespace magics
#endif
