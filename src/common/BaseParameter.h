/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file BaseParameter.h
    \brief Definition of Parameter base class.

    Magics Team - ECMWF 2004

    Started: Jan 2004

    Changes:

*/

#ifndef BaseParameter_H
#define BaseParameter_H


#include "MagException.h"
#include "Matrix.h"
#include "PaperPoint.h"
#include "magics.h"

#ifdef LATER
#include <grib_api.h>
typedef grib_handle* GribHandlePtr;
#endif


#include "MagLog.h"


namespace magics {

class MistmatchType : public MagicsException {
public:
    MistmatchType(const string& name, const string& type, const string& wait) :
        MagicsException("Parameter " + name + ": type mismatch -> type received \"" + type + "\", expected type \"" +
                        wait + "\" - setting ignored!") {}
};


class BaseParameter {
public:
    BaseParameter(const string& name);
    virtual ~BaseParameter();
    virtual void reset()                        = 0;
    virtual void setLocal(const BaseParameter*) = 0;
    virtual void resetLocal()                   = 0;
    virtual BaseParameter* clone()              = 0;
    const string& name() const { return name_; }

    virtual void set(const double&) { throw MistmatchType(name_, "real", type()); }
    virtual void setLocal(const double&) { throw MistmatchType(name_, "real", type()); }
    virtual void get(double&) const { throw MistmatchType(name_, "real", type()); }

    virtual void set(bool) { throw MistmatchType(name_, "bool", type()); }
    virtual void setLocal(bool) { throw MistmatchType(name_, "bool", type()); }
    virtual void get(bool&) const { throw MistmatchType(name_, "bool", type()); }

    virtual void set(const magvector<double>&) { throw MistmatchType(name_, "array of reals", type()); }
    virtual void setLocal(const magvector<double>&) { throw MistmatchType(name_, "array of reals", type()); }
    virtual void get(magvector<double>&) const { throw MistmatchType(name_, "array of reals", type()); }

    virtual void set(const int&) { throw MistmatchType(name_, "integer", type()); }
    virtual void setLocal(const int&) { throw MistmatchType(name_, "integer", type()); }
    virtual void get(int&) const { throw MistmatchType(name_, "integer", type()); }

    virtual void set(const magvector<int>&) { throw MistmatchType(name_, "integer", type()); }
    virtual void setLocal(const magvector<int>&) { throw MistmatchType(name_, "integer", type()); }
    virtual void get(magvector<int>&) const { throw MistmatchType(name_, "integer", type()); }

    virtual void set(const magvector<long int>&) { throw MistmatchType(name_, "long integer", type()); }
    virtual void setLocal(const magvector<long int>&) { throw MistmatchType(name_, "long integer", type()); }
    virtual void get(magvector<long int>&) const { throw MistmatchType(name_, "long integer", type()); }

    virtual void set(const string&) { throw MistmatchType(name_, "string", type()); }
    virtual void setLocal(const string&) { throw MistmatchType(name_, "string", type()); }
    virtual void get(string&) const { throw MistmatchType(name_, "string", type()); }

    virtual void set(const magvector<string>&) { throw MistmatchType(name_, "stringarray", type()); }
    virtual void setLocal(const magvector<string>&) { throw MistmatchType(name_, "stringarray", type()); }
    virtual void get(magvector<string>&) const { throw MistmatchType(name_, "stringarray", type()); }

    virtual void set(LineStyle) { throw MistmatchType(name_, "LineStyle", type()); }
    virtual void setLocal(LineStyle) { throw MistmatchType(name_, "LineStyle", type()); }
    virtual void get(LineStyle&) const { throw MistmatchType(name_, "LineStyle", type()); }

    virtual void set(DisplayType) { throw MistmatchType(name_, "LineStyle", type()); }
    virtual void setLocal(DisplayType) { throw MistmatchType(name_, "LineStyle", type()); }
    virtual void get(DisplayType&) const { throw MistmatchType(name_, "LineStyle", type()); }

    virtual void set(Justification) { throw MistmatchType(name_, "Justification", type()); }
    virtual void setLocal(Justification) { throw MistmatchType(name_, "LineStyle", type()); }
    virtual void get(Justification&) const { throw MistmatchType(name_, "Justification", type()); }

    virtual void set(ListPolicy) { throw MistmatchType(name_, "ListPolicy", type()); }
    virtual void setLocal(ListPolicy) { throw MistmatchType(name_, "LineStyle", type()); }
    virtual void get(ListPolicy&) const { throw MistmatchType(name_, "ListPolicy", type()); }

    virtual void set(AxisAutomaticSetting) { throw MistmatchType(name_, "AxisAutomaticSetting", type()); }
    virtual void setLocal(AxisAutomaticSetting) { throw MistmatchType(name_, "AxisAutomaticSetting", type()); }
    virtual void get(AxisAutomaticSetting&) const { throw MistmatchType(name_, "AxisAutomaticSetting", type()); }

    virtual void set(ArrowPosition) { throw MistmatchType(name_, "ArrowPosition", type()); }
    virtual void setLocal(ArrowPosition) { throw MistmatchType(name_, "ArrowPosition", type()); }
    virtual void get(ArrowPosition&) const { throw MistmatchType(name_, "ArrowPosition", type()); }

    virtual void set(const Matrix&) { throw MistmatchType(name_, "Matrix", type()); }
    virtual void setLocal(const Matrix&) { throw MistmatchType(name_, "Matrix", type()); }
    virtual void get(Matrix&) const { throw MistmatchType(name_, "Matrix", type()); }

    string getType(const string&) const { return "string"; }
    string getType(const int&) const { return "integer"; }
    string getType(const double&) const { return "real"; }
    string getType(const magvector<string>&) const { return "array of string"; }
    string getType(const magvector<int>&) const { return "array of integer"; }
    string getType(const magvector<long int>&) const { return "array of long integer"; }
    string getType(const magvector<double>&) const { return "array of real"; }
    string getType(LineStyle) const { return "LineStyle"; }
    string getType(AxisAutomaticSetting) const { return "AxisAutomaticSetting"; }
    string getType(Justification) const { return "Justification"; }
    string getType(ArrowPosition) const { return "ArrowPosition"; }
    string getType(Matrix) const { return "2DMatrix"; }

    static Justification justification(const std::string&);
    static const std::string& justification(const Justification&);

    static LineStyle lineStyle(const std::string&);
    static const std::string& lineStyle(const LineStyle&);

    static Position position(const std::string&);
    static const std::string& position(const Position&);

    static ListPolicy listPolicy(const std::string&);
    static const std::string& listPolicy(const ListPolicy&);

    static AxisAutomaticSetting axisAutomaticSetting(const std::string&);
    static const std::string& axisAutomaticSetting(const AxisAutomaticSetting&);

    static DisplayType displayType(const std::string&);
    static const std::string& displayType(const DisplayType&);

    static GraphicsFormat graphicsFormat(const std::string&);
    static const std::string& graphicsFormat(const GraphicsFormat&);

    static Hemisphere hemisphere(const std::string&);
    static const std::string& hemisphere(const Hemisphere&);

    static ArrowPosition arrowPosition(const std::string&);
    static const std::string& arrowPosition(const ArrowPosition&);

    static VerticalAlign verticalAlign(const std::string&);
    static const std::string& verticalAlign(const VerticalAlign&);

    virtual string type() const = 0;

protected:
    virtual void print(ostream&) const;
    string name_;

private:
    // No copy allowed
    BaseParameter(const BaseParameter&);
    BaseParameter& operator=(const BaseParameter&);

    // -- Friends
    friend ostream& operator<<(ostream& s, const BaseParameter& p) {
        p.print(s);
        return s;
    }
};

}  // namespace magics
#endif
