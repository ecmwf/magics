/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Colour.h
    \brief Definition of Colour class.

    Magics Team - ECMWF 2004

    Started by Sylvie Lamy-Thepaut ECMWF 2002

    Changes:

    Jan-2004 Stephan: Adopt this class to Magics++ 0.1 (namespace etc.)

*/
#ifndef Colour_H
#define Colour_H

#include <sstream>
#include "Factory.h"
#include "MagException.h"
#include "MagTranslator.h"
#include "magics.h"

using std::istream;

namespace magics {


struct Hsl;
class XmlNode;

/*! \struct Rgb
    \brief Struct expressing colour values in RGB.



*/
struct BadRgbFormat : public MagicsException {
    BadRgbFormat(const std::string& value) : MagicsException("BadRgbFormat: [" + value + "]") {}
    ~BadRgbFormat() override {}
};


struct Rgb {
    Rgb(float red = 1., float green = 1., float blue = 1., float alpha = 1.) :
        red_(red), green_(green), blue_(blue), alpha_(alpha) {}
    Rgb(const string&);

    bool operator==(const Rgb other) const {
        return (red_ == other.red_ && green_ == other.green_ && blue_ == other.blue_ && alpha_ == other.alpha_);
    }
    float red_;
    float green_;
    float blue_;
    float alpha_;
    Hsl hsl() const;
    friend ostream& operator<<(ostream& s, const Rgb& p) {
        s << "RGB(" << p.red_ << ", " << p.green_ << ", " << p.blue_ << ", " << p.alpha_ << ")";
        return s;
    }
    friend istream& operator>>(istream& s, Rgb& p);
};

struct BadHslFormat : public MagicsException {
    BadHslFormat(const std::string& value) : MagicsException("BadHslFormat: [" + value + "]") {}
    ~BadHslFormat() override {}
};

struct Hsl {
    Hsl(float hue = 0., float saturation = 1., float light = 1., float alpha = 1.) :
        hue_(hue), saturation_(saturation), light_(light), alpha_(alpha) {}
    Hsl(const string&);

    bool operator==(const Hsl& other) const {
        return (hue_ == other.hue_ && saturation_ == other.saturation_ && light_ == other.light_ &&
                alpha_ == other.alpha_);
    }
    float hue_;
    float saturation_;
    float light_;
    float alpha_;

    Rgb rgb() const;


    // -- Friends
    friend ostream& operator<<(ostream& s, const Hsl& p) {
        s << "HSL(" << p.hue_ << ", " << p.saturation_ << ", " << p.light_ << ", " << p.alpha_ << ")";
        return s;
    }
    friend istream& operator>>(istream& s, Hsl& p);
};

/*! \class Colour
    \brief Class to express a colour.

    This class holds colour values in RGB (for this it uses Rgb ).



*/
class Colour {
public:
    // -- Contructors
    Colour();
    Colour(const string&);
    Colour(float red, float green, float blue, float alpha = 1.);
    Colour(Rgb&);
    Colour(Hsl&);

    // -- Destructor
    ~Colour();

    void set(const map<string, string>&){};
    void set(const XmlNode&);
    Colour* clone() const { return new Colour(*this); }
    bool automatic() const { return automatic_; }
    bool none() const;
    bool white() const {
        return (red() == 1 && blue() == 1 && green() == 1) || (red() == 256 && blue() == 256 && green() == 256);
    }
    // -- Methods
    float red() const { return rgb_.red_; }
    float blue() const { return rgb_.blue_; }
    float green() const { return rgb_.green_; }
    float alpha() const { return rgb_.alpha_; }
    string name() const { return name_; }
    string rgb() const;

    bool operator==(const Colour& other) const { return other.rgb_ == rgb_; }

    bool operator==(const string& other) const { return lowerCase(other) == lowerCase(name_); }
    bool operator<(const Colour& other) const { return name_ < other.name_; }

    Hsl hsl() const;
    operator Rgb() const { return rgb_; }

    void setColour(const string&);
    void setColour(float, float, float, float = 1.);
    void setAlpha(float);
    void scaleLight(float);
    static bool valid(const string& name);

protected:
    void print(ostream&) const;

private:
    // -- Members
    static std::map<string, Rgb> colours_;
    static void init();
    Rgb rgb_;
    string name_;
    bool automatic_;

    // -- Friends
    friend ostream& operator<<(ostream& s, const Colour& p) {
        p.print(s);
        return s;
    }
};

template <>
class MagTranslator<string, Colour> {
public:
    Colour* operator()(const string& val);
    Colour* magics(const string& param) {
        string val;
        ParameterManager::get(param, val);
        return (*this)(val);
    }
};

}  // namespace magics
#endif
