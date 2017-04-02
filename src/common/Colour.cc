/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file Colour.cc
    \brief Implementation of Colour class.

    Magics Team - ECMWF 2004

    Started by Sylvie Lamy-Thepaut ECMWF 2002

    Changes:


*/
#include "Colour.h"
#include "MagLog.h"
#include "BaseParameter.h"
#include "XmlNode.h"

/*! \defgroup colours Colours

   \section howToDefine How can colours be defined?
   
   There are more than one way how colour values can be defined within Magics++
   
   - RGB
   - HSL
   
   \todo Where are the web colours defined?
   
   MagException of type BadRGBFormat and BadHSLFormat are thrown in case
   values look wrong.
*/


using namespace magics;

std::map<string, Rgb> Colour::colours_;



Colour::Colour() : rgb_(1., 0., 0., 1.), automatic_(false)
{
	init();
}

Colour::Colour(Rgb& rgb) : rgb_(rgb),  automatic_(false)
{ 
	name_ = tostring(rgb_);
}

Colour::Colour(Hsl& hsl) : rgb_(hsl.rgb()), automatic_(false) 
{ 
	name_ = tostring(rgb_);
}

Colour::Colour(float red, float green, float blue, float alpha) : 
	rgb_(red, green, blue, alpha), automatic_(false)
{
	ostringstream name;
	name << "RGB(" << red <<  "," << green << "," << blue << "," << alpha << ")" << "\n";
	name_ = name.str();
}

Colour::Colour(const string& name) : name_(lowerCase(name)), automatic_(false)
{
	if ( name == "automatic" ) automatic_ = true;
	init();
	try {
		setColour(name);
	}
	catch (BadHslFormat& e)
	{
		name_ = "red";
		rgb_ = Rgb(1., 0., 0.,1.);
	}
}

void Colour::set(const XmlNode& xml)
{
	MagLog::debug() << "set Colour-->" << xml << "\n";
	try {
		setColour(xml.data());
	}
	catch (BadHslFormat& e)
	{
		name_ = "red";
		rgb_ = Rgb(1., 0., 0.,1.);	        	
	}
}

void Colour::setColour(const string& name) 
{
	if ( name.empty() )
	{
		MagLog::warning() << "No colour name given. Red used.\n";
		name_ = "red";
		rgb_ = Rgb(1., 0., 0.);
		MagLog::broadcast();
		return;
	}
	try {
	// get rid of white space...
	string::size_type first = name.find_first_not_of(" ");  
	string::size_type last = name.find_last_not_of(" ");  

	string find = name.substr(first,  last-first+1);    
	name_ = lowerCase(find);

	map<string, Rgb>::iterator colour = colours_.find(name_);
	if (colour == colours_.end())
	{
		string prefix = name_.substr(0,3);

		if ( magCompare(prefix, "rgb") )
		{
			rgb_ = Rgb(name_);
		}
		else if ( magCompare(prefix, "hsl") )
		{
			Hsl hsl(name);
			rgb_ = hsl.rgb();
		}
		else if ( magCompare(name_.substr(0, 1), "#") )
         
		{
            // Web format should on 7 characher or 4 
            if ( name_.size() == 7 ) { 
			    float red = strtol(name_.substr(1, 2).c_str(), 0, 16)/256.;
			    float green = strtol(name_.substr(3, 2).c_str(), 0, 16)/256.;
			    float blue = strtol(name_.substr(5, 2).c_str(), 0, 16)/256.;
			    rgb_ = Rgb(red, green, blue, 1.);	
            }
            else if ( name_.size() == 4) {
                float red = strtol(name_.substr(1, 1).c_str(), 0, 16)/256.;
			    float green = strtol(name_.substr(2, 1).c_str(), 0, 16)/256.;
			    float blue = strtol(name_.substr(3, 1).c_str(), 0, 16)/256.;
			    rgb_ = Rgb(red, green, blue, 1.);
            }
            else {
                MagLog::warning() << "The color [" << name_ << "] is not valid. Red used.\n";
			    name_ = "red";
			    rgb_ = Rgb(1., 0., 0.,1.);	
            }
            		
		}
		else
		{
			MagLog::warning() << "The color [" << name_ << "] is not valid. Red used.\n";
			name_ = "red";
			rgb_ = Rgb(1., 0., 0.,1.);	
		}
	}
	else rgb_ = (*colour).second;
	}
	catch (...) {
		MagLog::warning() << " Invalid colour name " << name  << ". Red used.\n";
				name_ = "red";
				rgb_ = Rgb(1., 0., 0.);
				MagLog::broadcast();
				return;
	}
}

void Colour::setColour(float red, float green, float blue, float alpha)
{
	ostringstream name;

	name << "RGB(" << red <<  "," << green << "," << blue<< "," << alpha << ")" << "\n";
	name_ = name.str();
	rgb_ = Rgb(red, green, blue, alpha);
}

void Colour::setAlpha(float alpha)
{
	ostringstream name;
	
	name << "RGB(" << red() <<  "," << green() << "," << blue()<< "," << alpha << ")" << "\n";
	name_ = name.str();
	rgb_ = Rgb(red(), green(), blue(), alpha);
}

void Colour::scaleLight(float scale_factor)
{	
	Hsl hsl=rgb_.hsl();
	hsl.light_*=scale_factor;
	if(hsl.light_ > 1) hsl.light_=1.;
	Rgb rgb=hsl.rgb();

	setColour(rgb.red_,rgb.green_,rgb.blue_,rgb_.alpha_);
}


Colour::~Colour() {}
	
void Colour::print(ostream& out)  const
{
	
	out  << "["<<rgb_.red_<<", "<<rgb_.green_<< ", "<< rgb_.blue_ << ", " << rgb_.alpha_<<"] ";
}

Hsl Colour::hsl() const 
{ 
    return rgb_.hsl();
}

Hsl Rgb::hsl() const 
{
    Hsl hsl;
    float min = std::min( red_, std::min(green_, blue_) );    //Min. value of RGB
    float max = std::max( red_, std::max(green_, blue_) );    //Max. value of RGB
    float diff = max - min;             //Delta RGB value

    hsl.light_ = ( max + min ) / 2;
    hsl.alpha_ = alpha_;

    if ( diff == 0 )                     //This is a gray, no chroma...
    {
        hsl.hue_ = 0;                                //HSL results = From 0 to 1
        hsl.saturation_ = 0;
    }
    else                                    //Chromatic data...
    {
        if ( hsl.light_ < 0.5 ) hsl.saturation_ = diff / ( max + min );
        else   hsl.saturation_ = diff / ( 2 - max - min );

        float red   = ( ( ( max - red_)  / 6 ) + ( diff / 2 ) ) / diff;
        float green = ( ( ( max - green_ ) / 6 ) + ( diff / 2 ) ) / diff;
        float blue  = ( ( ( max - blue_ ) / 6 ) + ( diff / 2 ) ) / diff;

        if      ( red_ == max ) hsl.hue_ = blue - green; 
        else if ( green_ == max ) hsl.hue_ = ( 1. / 3. ) + red - blue;
        else if ( blue_ == max ) hsl.hue_ = ( 2. / 3. ) + green - red;

        if ( hsl.hue_ < 0 )  hsl.hue_ += 1;
        if ( hsl.hue_ > 1 )  hsl.hue_ -= 1;
        hsl.hue_ *= 360.;
    }
    return hsl;
}

inline float hue2rgb(float v1, float v2, float vH )             //Function Hue_2_RGB
{
   if ( vH < 0 ) vH += 1;
   if ( vH > 1 ) vH -= 1;
   if ( ( 6 * vH ) < 1 ) return ( v1 + ( v2 - v1 ) * 6 * vH );
   if ( ( 2 * vH ) < 1 ) return ( v2 );
   if ( ( 3 * vH ) < 2 ) return ( v1 + ( v2 - v1 ) * ( ( 2. / 3.) - vH ) * 6 );
   return v1;
}

Rgb Hsl::rgb() const 
{
    Rgb rgb;
    rgb.alpha_ = alpha_;
    float var1, var2;
    if ( saturation_ == 0 ) {
        rgb.red_ = light_;
        rgb.green_ = light_;
        rgb.blue_ = light_;
    }
    else
    {
        if ( light_ < 0.5 ) var2 = light_ * ( 1 + saturation_ );
        else  var2 = ( light_ + saturation_ ) - ( saturation_ * light_ );

        var1 = 2 * light_ - var2;

        rgb.red_ = hue2rgb( var1, var2, (hue_/360.) + ( 1. / 3. ) );
        rgb.green_ = hue2rgb( var1, var2, (hue_/360.) );
        rgb.blue_ = hue2rgb( var1, var2, (hue_/360.) - ( 1. / 3.) );
    }
    if (rgb.red_ < 0.000001) rgb.red_= 0.;
    if (rgb.green_ < 0.000001) rgb.green_= 0.;
    if (rgb.blue_ < 0.000001) rgb.blue_= 0.;
    return rgb;
}


bool Colour::valid(const string& name)
{
	init();
	Colour colour;
	try {
		colour.setColour(name);
	}
	catch (BadHslFormat& e)
	{
	        return false;
	}
	return true;
}


void Colour::init() {
	if (!colours_.empty()) return;
	 colours_["automatic"] = Rgb(0., 0., 0.); // NEEDS FIXING 
	 colours_["none"] = Rgb(-1., -1., -1.); 
	 colours_["background"] = Rgb(1., 1., 1.); 
	 colours_["foreground"] = Rgb(0., 0., 0.); 
	 colours_["ecmwf_blue"] = Rgb(0.25, 0.43, 0.7); 
	 colours_["red"] = Rgb(1.0000, 0.0000, 0.0000); 
	 colours_["green"] = Rgb(0.0000, 1.0000, 0.0000); 
	 colours_["blue"] = Rgb(0.0000, 0.0000, 1.0000);
	 colours_["yellow"] = Rgb(1.0000, 1.0000, 0.0000); 
	 colours_["cyan"] = Rgb(0.0000, 1.0000, 1.0000); 
	 colours_["magenta"] = Rgb(1.0000, 0.0000, 1.0000); 
	 colours_["black"] = Rgb(0.0000, 0.0000, 0.0000); 
	 colours_["avocado"] = Rgb(0.4225, 0.6500, 0.1950); 
	 colours_["beige"] = Rgb(0.8500, 0.7178, 0.4675); 
	 colours_["brick"] = Rgb(0.6000, 0.0844, 0.0300); 
	 colours_["brown"] = Rgb(0.4078, 0.0643, 0.0000); 
	 colours_["burgundy"] = Rgb(0.5000, 0.0000, 0.1727); 
	 colours_["charcoal"] = Rgb(0.2000, 0.2000, 0.2000); 
	 colours_["chestnut"] = Rgb(0.3200, 0.0112, 0.0000); 
	 colours_["coral"] = Rgb(0.9000, 0.2895, 0.2250); 
	 colours_["cream"] = Rgb(1.0000, 0.8860, 0.6700); 
	 colours_["evergreen"] = Rgb(0.0000, 0.4500, 0.2945); 
	 colours_["gold"] = Rgb(0.7500, 0.5751, 0.0750); 
	 colours_["grey"] = Rgb(0.7000, 0.7000, 0.7000); 
	 colours_["khaki"] = Rgb(0.5800, 0.4798, 0.2900); 
	 colours_["kelly_green"] = Rgb(0.0000, 0.5500, 0.1900); 
	 colours_["lavender"] = Rgb(0.6170, 0.4070, 0.9400); 
	 colours_["mustard"] = Rgb(0.6000, 0.3927, 0.0000); 
	 colours_["navy"] = Rgb(0.0000, 0.0000, 0.4000); 
	 colours_["ochre"] = Rgb(0.6800, 0.4501, 0.0680); 
	 colours_["olive"] = Rgb(0.3012, 0.3765, 0.0000); 
	 colours_["peach"] = Rgb(0.9400, 0.4739, 0.3788); 
	 colours_["pink"] = Rgb(0.9000, 0.3600, 0.4116); 
	 colours_["rose"] = Rgb(0.8000, 0.2400, 0.4335); 
	 colours_["rust"] = Rgb(0.7000, 0.2010, 0.0000); 
	 colours_["sky"] = Rgb(0.4500, 0.6400, 1.0000); 
	 colours_["tan"] = Rgb(0.4000, 0.3309, 0.2000); 
	 colours_["tangerine"] = Rgb(0.8784, 0.4226, 0.0000); 
	 colours_["turquoise"] = Rgb(0.1111, 0.7216, 0.6503); 
	 colours_["violet"] = Rgb(0.4823, 0.0700, 0.7000); 
	 colours_["reddish_purple"] = Rgb(1.0000, 0.0000, 0.8536); 
	 colours_["purple_red"] = Rgb(1.0000, 0.0000, 0.5000); 
	 colours_["purplish_red"] = Rgb(1.0000, 0.0000, 0.2730); 
	 colours_["orangish_red"] = Rgb(1.0000, 0.0381, 0.0000); 
	 colours_["red_orange"] = Rgb(1.0000, 0.1464, 0.0000); 
	 colours_["reddish_orange"] = Rgb(1.0000, 0.3087, 0.0000); 
	 colours_["orange"] = Rgb(1.0000, 0.5000, 0.0000); 
	 colours_["yellowish_orange"] = Rgb(1.0000, 0.6913, 0.0000); 
	 colours_["orange_yellow"] = Rgb(1.0000, 0.8536, 0.0000); 
	 colours_["orangish_yellow"] = Rgb(1.0000, 0.9619, 0.0000); 
	 colours_["greenish_yellow"] = Rgb(0.8536, 1.0000, 0.0000); 
	 colours_["yellow_green"] = Rgb(0.5000, 1.0000, 0.0000); 
	 colours_["yellowish_green"] = Rgb(0.1464, 1.0000, 0.0000); 
	 colours_["bluish_green"] = Rgb(0.0000, 1.0000, 0.5000); 
	 colours_["blue_green"] = Rgb(0.0000, 1.0000, 1.0000); 
	 colours_["greenish_blue"] = Rgb(0.0000, 0.5000, 1.0000); 
	 colours_["purplish_blue"] = Rgb(0.1464, 0.0000, 1.0000); 
	 colours_["blue_purple"] = Rgb(0.5000, 0.0000, 1.0000); 
	 colours_["bluish_purple"] = Rgb(0.8536, 0.0000, 1.0000); 
	 colours_["purple"] = Rgb(1.0000, 0.0000, 1.0000); 
	 colours_["white"] = Rgb(1.0000, 1.0000, 1.0000); 
	 colours_["undefined"] = Rgb(-1., -1., -1.);
}

#include "BackgroundColour.h"
Colour* MagTranslator<string, Colour>::operator()(const string& val )
{
 	 if (Colour::valid(val) ) return new Colour(val);
 	 if ( val == "colour" ) return new Colour();
 	 if ( val == "background_colour" ) return new BackgroundColour();
 	 throw NoFactoryException(val);
}

Rgb::Rgb(const string& name): red_(1.), green_(1.), blue_(1.),alpha_(1.)
{
  istringstream in(name);
  stringbuf token;
  in >> ws;
  in.get(token, '(');

  if (in.eof()) throw BadRgbFormat();

  if (magCompare(token.str(), "rgb"))
  {
	      in.ignore(name.length(), '(');
	      in >> red_; 

	      in.ignore(name.length(), ',');
	      if (in.eof()) throw BadRgbFormat();
	      in >> green_;

	      in.ignore(name.length(), ',');
	      if (in.eof()) throw BadRgbFormat();
	      in >> blue_;


  }
  else if (magCompare(token.str(), "rgba")) {
	      in.ignore(name.length(), '(');
	      in >> red_;



	      in.ignore(name.length(), ',');
	      if (in.eof()) throw BadRgbFormat();	    
	      in >> green_;


	      in.ignore(name.length(), ',');
	      if (in.eof()) throw BadRgbFormat();	    
	      in >> blue_; 

	      in.ignore(name.length(), ',');
	      if (in.eof()) throw BadRgbFormat();	    
	      in >> alpha_; 
	      if (alpha_ <0 || alpha_>1) throw BadRgbFormat();
    }
    else 
          throw BadRgbFormat();
    if ( red_ > 1 || green_ > 1 || blue_ > 1) {
    	// WE are using the second convention ( Colour defined between, 0 and 1)
    	red_ = red_/256;

    	green_ = green_/256;
    	blue_ = blue_/256.;
    }
    if (red_ <0 || red_>1) throw BadRgbFormat();
    if (green_ <0 || green_>1) throw BadRgbFormat();
    if (blue_ <0 || blue_>1) throw BadRgbFormat();
}


Hsl::Hsl(const string& name)
{
    istringstream in(name);
    stringbuf token;
    in >> ws;
    in.get(token, '(');

    if (in.eof()) throw BadHslFormat();

    if (magCompare(token.str(),"HSL")) {

        in.ignore(name.length(), '(');
        in >> hue_; 
        if (hue_ <0 || hue_>360) throw BadHslFormat();

        in.ignore(name.length(), ',');
        if (in.eof()) throw BadHslFormat();	    
        in >> saturation_;
        if ( saturation_ <0 || saturation_>1) throw BadHslFormat();

        in.ignore(name.length(), ',');
        if (in.eof()) throw BadHslFormat();	    
        in >> light_; 
        if ( light_ <0 || light_>1) throw BadHslFormat();
        alpha_=1.;
    }
    else if (magCompare(token.str(),"HSLA")) {
        in.ignore(name.length(), '(');
        in >> hue_; 
        if (hue_ <0 || hue_>360) throw BadHslFormat();

        in.ignore(name.length(), ',');
        if (in.eof()) throw BadHslFormat();
        in >> saturation_;
        if ( saturation_ <0 || saturation_>1) throw BadHslFormat();

        in.ignore(name.length(), ',');
        if (in.eof()) throw BadHslFormat();
        in >> light_; 
        if ( light_ <0 || light_>1) throw BadHslFormat();

        in.ignore(name.length(), ',');
        if (in.eof()) throw BadHslFormat();
        in >> alpha_; 
        if ( alpha_ <0 || alpha_>1) throw BadHslFormat();
    }
    else throw BadHslFormat();
}


istream& operator>>(istream& s, Rgb& p)
{
	string token;
	s.width(3);
	s >> token;

	if (token != "RGB") throw BadRgbFormat();

	s.ignore(256, '(');
	s >> p.red_; 
	if (p.red_ <0 || p.red_>1) throw BadRgbFormat();
		
	s.ignore(256, ',');
	s >> p.green_;
	if (p.green_ <0 || p.green_>1) throw BadRgbFormat();
	s.ignore(256, ',');

	s >> p.blue_; 
	if (p.blue_ <0 || p.blue_>1) throw BadRgbFormat();

	return s; 
}

istream& operator>>(istream& s, Hsl& p)
{
	string token;
	s.width(3);
	s >> token;

	if (token != "HSL") throw BadHslFormat();

	s.ignore(256, '(');
	s >> p.hue_; 
	if (p.hue_ <0 || p.hue_>360) throw BadHslFormat();

	s.ignore(256, ',');
	s >> p.saturation_;
	if (p.saturation_ <0 || p.saturation_>1) throw BadHslFormat();
	s.ignore(256, ',');

	s >> p.light_; 
	if (p.light_ <0 || p.light_>1) throw BadHslFormat();
	return s; 
}
bool Colour::none() const
{
	static Colour none("none");
	return *this == none;
}
