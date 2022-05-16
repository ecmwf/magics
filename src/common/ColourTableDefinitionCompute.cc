/*
 * (C) Copyright 1996-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ColourTableDefinitionCompute.cc
    \brief Implementation of the Template class ColourTableDefinitionCompute.

    Magics Team - ECMWF 2005

    Started: Mon 4-Jul-2005

    Changes:

*/


#include "ColourTableDefinitionCompute.h"
#include "XmlNode.h"

using namespace magics;
ColourTableDefinitionCompute::ColourTableDefinitionCompute(): 
    direction_("clockwise"), method_("rgb") {
     methods_["anti_clockwise"] = &ColourTableDefinitionCompute::hsl;
    methods_["clockwise"]      = &ColourTableDefinitionCompute::hsl;
    methods_["linear"]         = &ColourTableDefinitionCompute::linear;
    methods_["hsl"]    = &ColourTableDefinitionCompute::hsl;
    methods_["linear"] = &ColourTableDefinitionCompute::linear;
    methods_["rgb"]    = &ColourTableDefinitionCompute::linear;
    methods_["hcl"]    = &ColourTableDefinitionCompute::hcl;
    dynamicMethods_["normal"]    = &ColourTableDefinitionCompute::dynamic_normal;
    dynamicMethods_["divergent"] = &ColourTableDefinitionCompute::dynamic_divergent;
}


ColourTableDefinitionCompute::ColourTableDefinitionCompute(const string& method, const string& direction): 
    direction_(direction), method_(method) {
    methods_["anti_clockwise"] = &ColourTableDefinitionCompute::hsl;
    methods_["clockwise"]      = &ColourTableDefinitionCompute::hsl;
    methods_["linear"]         = &ColourTableDefinitionCompute::linear;
    methods_["hsl"]    = &ColourTableDefinitionCompute::hsl;
    methods_["linear"] = &ColourTableDefinitionCompute::linear;
    methods_["rgb"]    = &ColourTableDefinitionCompute::linear;
    methods_["hcl"]    = &ColourTableDefinitionCompute::hcl;
    dynamicMethods_["normal"]    = &ColourTableDefinitionCompute::dynamic_normal;
    dynamicMethods_["divergent"] = &ColourTableDefinitionCompute::dynamic_divergent;
}

ColourTableDefinitionCompute::ColourTableDefinitionCompute(const string& min, const string& max, const string& method,
                                                           const string& direction) :
    minColour_(min), maxColour_(max), direction_(direction), method_(method) {
    methods_["hsl"]    = &ColourTableDefinitionCompute::hsl;
    methods_["linear"] = &ColourTableDefinitionCompute::linear;
    methods_["rgb"]    = &ColourTableDefinitionCompute::linear;
    methods_["hcl"]    = &ColourTableDefinitionCompute::hcl;
    dynamicMethods_["normal"]    = &ColourTableDefinitionCompute::dynamic_normal;
    dynamicMethods_["divergent"] = &ColourTableDefinitionCompute::dynamic_divergent;
}


ColourTableDefinitionCompute::~ColourTableDefinitionCompute() {}

/*!
 Class information are given to the output-stream.
*/
void ColourTableDefinitionCompute::print(ostream& out) const {
    out << "ColourTableDefinitionCompute[";
    out << "]";
}

void ColourTableDefinitionCompute::set(const ColourTableDefinitionComputeInterface& attributes) {
    minColour_ = attributes.getMin();
    maxColour_ = attributes.getMax();
    direction_ = attributes.getDirection();
    method_ = "hsl";
}

void ColourTableDefinitionCompute::set(const XmlNode& node) {
    direction_ = node.getAttribute("direction");
    MagLog::warning() << "ColourTableDefinitionCompute::set(const XmlNode&): to be implemented\n";

    for (auto& elt : node.elements()) {
        if (magCompare(elt->name(), "min_colour")) {
            minColour_ = Colour(elt->data());
        }
        if (magCompare(elt->name(), "max_colour")) {
            maxColour_ = Colour(elt->data());
        }
    }
}


void ColourTableDefinitionCompute::hsl(ColourTable& table, int nb) {
    float step_hue;
    float step_light;
    float step_alpha;
    float step_sat;
    Hsl hmin = minColour_.hsl();
    Hsl hmax = maxColour_.hsl();


    step_sat   = (hmax.saturation_ - hmin.saturation_) / (nb - 2);
    step_light = (hmax.light_ - hmin.light_) / (nb - 2);
    step_alpha = (hmax.alpha_ - hmin.alpha_) / (nb - 2);


    if (magCompare(direction_, "anti_clockwise")) {
        if (hmax.hue_ < hmin.hue_)
            hmax.hue_ += 360;
        step_hue = (hmax.hue_ - hmin.hue_) / (nb - 2);
    }
    else {
        if (hmin.hue_ < hmax.hue_)
            hmin.hue_ += 360;
        step_hue = (hmax.hue_ - hmin.hue_) / (nb - 2);
    }

    /*
    if ( minColour_.white() ) {
        step_sat = 0;
        step_hue = 0;
        hmin.saturation_ = hmax.saturation_;
        hmin.hue_ = hmax.hue_;

    }

     if ( maxColour_.white() ) {
        step_sat = 0;
        step_hue = 0;


      }
    */


    // WE have nb levels : we need nb-1 colours!

    for (int i = 0; i < nb - 1; i++) {
        MagLog::dev() << "ColourTableDefinitionCompute::set->add-->" << Colour(hmin) << endl;
        table.push_back(Colour(hmin));
        hmin.saturation_ += step_sat;
        hmin.hue_ += step_hue;
        hmin.light_ += step_light;
        hmin.alpha_ += step_alpha;
    }
}


#include <math.h>
void ColourTableDefinitionCompute::hsl_shortest(ColourTable& table, int nb) {
    Hsl hmin = minColour_.hsl();
    Hsl hmax = maxColour_.hsl();


    float angle = fmod((hmax.hue_ - hmin.hue_ + 360.), 360.);


    if (angle > 180)
        direction_ = "clockwise";


    else
        direction_ = "anti_clockwise";


    hsl(table, nb);
}
#include <math.h>
void ColourTableDefinitionCompute::hsl_longest(ColourTable& table, int nb) {
    Hsl hmin = minColour_.hsl();
    Hsl hmax = maxColour_.hsl();


    float angle = fmod((hmax.hue_ - hmin.hue_ + 360.), 360.);


    if (angle > 180) {
        direction_ = "anti_clockwise";
    }
    else {
        direction_ = "clockwise";
    }

    hsl(table, nb);
}

void ColourTableDefinitionCompute::linear(ColourTable& table, int nb) {
    float step_red;
    float step_green;
    float step_blue;
    float step_alpha;

    step_red    = (maxColour_.red() - minColour_.red()) / (nb - 2);
    step_green  = (maxColour_.green() - minColour_.green()) / (nb - 2);
    step_blue   = (maxColour_.blue() - minColour_.blue()) / (nb - 2);
    step_alpha  = (maxColour_.alpha() - minColour_.alpha()) / (nb - 2);
    float red   = minColour_.red();
    float green = minColour_.green();
    float blue  = minColour_.blue();
    float alpha = minColour_.alpha();

    for (int i = 0; i < nb - 1; i++) {
        table.push_back(Colour(red, green, blue, alpha));

        red += step_red;
        green += step_green;
        blue += step_blue;
        alpha += step_alpha;
    }
}

void ColourTableDefinitionCompute::hcl(ColourTable& table, int nb) {
    float step_h;
    float step_c;
    float step_l;
    float step_alpha;

    float minh, minc, minl;
    float maxh, maxc, maxl;

    hcl(maxColour_, maxh, maxc, maxl);
    hcl(minColour_, minh, minc, minl);

    if (maxh == -1)
        maxh = minh;
    if (minh == -1)
        minh = maxh;

    if (magCompare(direction_, "shortest")) {
        float angle = fmod((maxh - minh + 360.), 360.);
        direction_  = (angle > 180) ? "clockwise" : "anti_clockwise";
    }

    if (magCompare(direction_, "longest")) {
        float angle = fmod((maxh - minh + 360.), 360.);
        direction_  = (angle < 180) ? "clockwise" : "anti_clockwise";
    }

    if (magCompare(direction_, "anti_clockwise")) {
        if (maxh < minh)
            maxh += 360;
    }
    else {
        if (minh < maxh)
            minh += 360;
    }

    step_h = (maxh - minh) / (nb - 2);
    step_c = (maxc - minc) / (nb - 2);
    step_l = (maxl - minl) / (nb - 2);

    step_alpha = (maxColour_.alpha() - minColour_.alpha()) / (nb - 2);

    float h     = minh;
    float c     = minc;
    float l     = minl;
    float alpha = minColour_.alpha();

    for (int i = 0; i < nb - 1; i++) {
        table.push_back(rgb(h, c, l, alpha));

        h += step_h;
        c += step_c;
        l += step_l;
        alpha += step_alpha;
    }
}


void ColourTableDefinitionCompute::set(ColourTable& table, int nb) {
    prepare();

    MagLog::dev() << "ColourTableDefinitionCompute::set->min-->" << minColour_ << endl;
    MagLog::dev() << "ColourTableDefinitionCompute::set->max-->" << maxColour_ << endl;
    MagLog::dev() << "nb interval-->" << nb << endl;

    if (nb == 1) {
        table.push_back(minColour_);
        return;
    }
    if (nb == 2) {
        table.push_back(minColour_);
        return;
    }
    if (nb == 3) {
        table.push_back(minColour_);
        table.push_back(maxColour_);
        return;
    }
    std::map<string, ComputeFunction>::iterator method = methods_.find(lowerCase(method_));
    if (method == methods_.end())
        hsl(table, nb);
    else
        (this->*method->second)(table, nb);
    
    
}

void ColourTableDefinitionCompute::dynamic_divergent(const stringarray& from, ColourTable& to, int nb)
{
    // Nb is the number of intervals!
    // We need nb-1 colours!
    stringarray left;
    stringarray right;
    string middle;
    int count = from.size();
    int nbcol = nb-1;


    // cout  << " dynamic_divergent " << count/2 << "-->nb col:" << nbcol << " " << nbcol/2 << endl;

    if ( count  % 2 == 0) {
        MagLog::warning() << "Can not create the palette " << endl;
        dynamic_normal(from, to, nb);
    }

    for ( int i = 0; i < count/2; i++) 
        left.push_back(from[i]);

    // cout << "left colours->" << left.size() << endl;
    
    middle = from[count/2];

    for (int i = (count/2)+1; i < count; i++)
        right.push_back(from[i]);
    
    // cout << "right colours->" << right.size() << endl;
    // cout << "Asking " << (nbcol/2) << " colours" << endl;
    if ( nbcol % 2 == 0) {
        dynamic_normal(left, to, (nbcol/2)+1);
        // cout << "after left " << to.size() << endl;
        dynamic_normal(right, to, (nb/2)+1);
    //     cout << "after right " << to.size() << endl;
    // 
    }
    else {
        
        dynamic_normal(left, to, (nbcol/2)+1);
        // cout << "after left " << to.size() << endl;
        to.push_pack(middle);
        // cout << "middle " << to.size() << endl;
        dynamic_normal(right, to, (nbcol/2)+1);
        // cout << "after right " << to.size() << endl;
    }

//   cout << "return " << to.size() << endl;

}


void ColourTableDefinitionCompute::dynamic_normal(const stringarray& from, ColourTable& to, int nb)
{
    // Nb is the number of intervals!
    // We need nb-1 colours!
    // cout << "dynamic_normal--> " << nb-1 << " colours" << endl;
    stringarray::const_iterator colour = from.begin();
    // Nb is the number of intervals!
    minColour_ = *colour;
    ++colour;
    int modulo = 0;
    int count = 0;
    int nbcols = (((nb-1)*from.size())/(from.size()-1)) + 1;
    for ( auto c = colour; c != from.end(); ++c) {
        maxColour_ = *c;
       
        ColourTable workingtable;
        set(workingtable, nbcols);

        
        for (int i = 0; i < workingtable.size()-1; ++i) {
            if ( !modulo ) {
                to.push_back(workingtable[i]);
                count++;
            }
            modulo++;
            if ( modulo ==   from.size() )
                modulo = 0;
        }
        minColour_ = maxColour_;
        
    }
    if ( count < nb -1 ) 
        to.push_pack(from.back());

// cout << "return " << to.size() << endl;
}

void ColourTableDefinitionCompute::set(const stringarray& from, ColourTable& to, 
        int nb, ColourListPolicy policy, const string& method)
{
   
    auto colour = from.begin();
    if (policy == ColourListPolicy::DYNAMIC) {
        auto helper = dynamicMethods_.find(method);
        if ( helper != dynamicMethods_.end() )
            (this->*helper->second)(from, to , nb);
        else {
            MagLog::warning() << "Method [" << method_ << "] not found , revertingto default" << endl;
            dynamic_normal(from, to , nb);
        }
            
        
        return;
    }
    
    for (int i = 0; i < nb - 1; i++) {
        if (colour == from.end()) {
            if (policy == ColourListPolicy::LASTONE) {
                to.push_back(Colour(from.back()));
            }
            else {
                colour = from.begin();
                to.push_back(Colour(*colour));
                colour++;
            }
        }
        else {
            to.push_back(Colour(*colour));
            colour++;
        }
    }
    


}


static const float toRadFactor = 3.14159265359 / 180.;
static const float toDegFactor = 180. / 3.14159265359;

// reference values for the XYZ colour model.Observer= 2 deg, illuminant= D65
static float REF_X = 95.047;
static float REF_Y = 100.000;
static float REF_Z = 108.883;

void ColourTableDefinitionCompute::hcl(const Colour& colour, float& h, float& c, float& l) {
    float x, y, z;
    rgbToXyz(colour.red(), colour.green(), colour.blue(), x, y, z);
    xyzToHcl(x, y, z, h, c, l);
    h *= 360;
}

Colour ColourTableDefinitionCompute::rgb(float h, float c, float l, float alpha) {
    float x, y, z;
    float r, g, b;

    hclToXyz(h / 360., c, l, x, y, z);
    xyzToRgb(x, y, z, r, g, b);
    return Colour(r, g, b, alpha);
}

void ColourTableDefinitionCompute::xyzToRgb(float x, float y, float z, float& r, float& g, float& b) {
    x = x / 100.;  // x from 0 to 95.047
    y = y / 100.;  // y from 0 to 100.000
    z = z / 100.;  // z from 0 to 108.883

    r = x * 3.2406 + y * (-1.5372) + z * (-0.4986);
    g = x * (-0.9689) + y * 1.8758 + z * 0.0415;
    b = x * 0.0557 + y * (-0.2040) + z * 1.0570;

    if (r > 0.0031308)
        r = 1.055 * pow(r, 1. / 2.4) - 0.055;
    else
        r = 12.92 * r;

    if (g > 0.0031308)
        g = 1.055 * pow(g, 1. / 2.4) - 0.055;
    else
        g = 12.92 * g;

    if (b > 0.0031308)
        b = 1.055 * pow(b, 1. / 2.4) - 0.055;
    else
        b = 12.92 * b;

    if (r > 1.)
        r = 1.;
    if (g > 1.)
        g = 1.;
    if (b > 1.)
        b = 1.;

    if (r < 0.)
        r = 0.;
    if (g < 0.)
        g = 0.;
    if (b < 0.)
        b = 0.;
}

void ColourTableDefinitionCompute::xyzToHcl(float x, float y, float z, float& h, float& c, float& l) {
    x /= REF_X;
    y /= REF_Y;
    z /= REF_Z;

    if (x > 0.008856)
        x = pow(x, 1. / 3.);
    else
        x = (7.787 * x) + (16. / 116.);

    if (y > 0.008856)
        y = pow(y, 1. / 3.);
    else
        y = (7.787 * y) + (16. / 116.);

    if (z > 0.008856)
        z = pow(z, 1. / 3.);
    else
        z = (7.787 * z) + (16. / 116.);

    l       = (116. * y) - 16.;
    float a = 500. * (x - y);
    float b = 200. * (y - z);

    h = atan2(b, a);

    if (h > 0)
        h = h * toDegFactor;
    else
        h = 360. + h * toDegFactor;

    h = h / 360.;
    c = sqrt(a * a + b * b);
}

void ColourTableDefinitionCompute::hclToXyz(float h, float c, float l, float& x, float& y, float& z) {
    float a = cos(360 * h * toRadFactor) * c;
    float b = sin(360 * h * toRadFactor) * c;

    y = (l + 16.) / 116.;
    x = a / 500. + y;
    z = y - b / 200.;

    if (pow(y, 3) > 0.008856)
        y = pow(y, 3);
    else
        y = (y - 16. / 116.) / 7.787;

    if (pow(x, 3) > 0.008856)
        x = pow(x, 3);
    else
        x = (x - 16. / 116.) / 7.787;

    if (z > 0.008856)
        z = pow(z, 3);
    else
        z = (z - 16. / 116.) / 7.787;

    x *= REF_X;
    y *= REF_Y;
    z *= REF_Z;
}

void ColourTableDefinitionCompute::rgbToXyz(float r, float g, float b, float& x, float& y, float& z) {
    if (r > 0.04045)
        r = pow((r + 0.055) / 1.055, 2.4);
    else
        r = r / 12.92;

    if (g > 0.04045)
        g = pow((g + 0.055) / 1.055, 2.4);
    else
        g = g / 12.92;

    if (b > 0.04045)
        b = pow((b + 0.055) / 1.055, 2.4);
    else
        b = b / 12.92;

    r *= 100;
    g *= 100;
    b *= 100;

    x = r * 0.4124 + g * 0.3576 + b * 0.1805;
    y = r * 0.2126 + g * 0.7152 + b * 0.0722;
    z = r * 0.0193 + g * 0.1192 + b * 0.9505;
}
