/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file MagicsParameter.h
    \brief Definition of the MagicsParameter template class.
    \author Meteorological Visualisation Section, ECMWF

    Started: Jan 2004

    Changes:

*/
#ifndef MagicsParameter_H
#define MagicsParameter_H

#include "BaseParameter.h"
#include "MagLog.h"
#include "magics.h"

namespace magics {

template <class T>
class MagicsParameter : public BaseParameter {
public:
    MagicsParameter(const string& name, const T& def) : BaseParameter(name), default_(def), value_(def) {}

    ~MagicsParameter() override {}

    void get(T& value) const override { value = value_; }
    void reset() override { value_ = default_; }

    BaseParameter* clone() override {
        return new MagicsParameter<T>(this->name_, this->default_);
    }  // FIXME: default or value?

    string type() const override { return getType(default_); }

    void set(const T& value) override { value_ = value; }

protected:
    void print(ostream& out) const override { out << name_ << "[" << value_ << ", " << default_ << "]"; }

    T default_;
    T value_;

private:
    // No copy allowed
    MagicsParameter(const MagicsParameter<T>&);
    MagicsParameter& operator=(const MagicsParameter&);

    // -- Friends
    friend ostream& operator<<(ostream& s, const MagicsParameter<T>& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics

#endif
