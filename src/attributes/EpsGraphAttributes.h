
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileEpsGraphAttributes.h
    \brief Definition of EpsGraph Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef EpsGraphAttributes_H
#define EpsGraphAttributes_H

#include "magics.h"
#include "Colour.h"
namespace magics {

class XmlNode;
class EpsGraphAttributes 
{
public:
//  --  constructor
    EpsGraphAttributes();
    
//  --  destructor
    virtual ~EpsGraphAttributes();
    
    virtual void set(const std::map<std::string, std::string>&);
    virtual void set(const XmlNode&);
    virtual void copy(const EpsGraphAttributes&);
    virtual bool accept(const std::string&);

    void setTag(const std::string& tag) { tag_ = tag; }

public:
	//  --  method
	virtual void print(std::ostream&) const;
	virtual void toxml(std::ostream& out) const;
	//  --  members:
	string tag_;
	string font_;
	double font_size_;
	string font_style_;
	int box_shift_;
	stringarray quantiles_colour_;
	int border_thickness_;
	int median_thickness_;
	double max_;
	string max_font_name_;
	string max_font_style_;
	double max_font_size_;
	double box_width_;
	bool whisker_;
	string legend_resolution_;
	string legend_control_text_;
	double legend_size_;
	string legend_forecast_text_;
	bool deterministic_;
	int deterministic_thickness_;
	string deterministic_legend_;
	bool control_;
	int control_thickness_;
	string control_legend_;
	bool legend_;
	bool grey_legend_;
	unique_ptr<Colour> font_colour_;
	unique_ptr<Colour> colour_;
	unique_ptr<Colour> right_colour_;
	unique_ptr<Colour> left_colour_;
	unique_ptr<Colour> border_colour_;
	unique_ptr<Colour> median_colour_;
	unique_ptr<Colour> max_font_colour_;
	unique_ptr<Colour> deterministic_colour_;
	LineStyle deterministic_style_;
	unique_ptr<Colour> control_colour_;
	LineStyle control_style_;
	 

private:
	friend ostream& operator<<(ostream& s,const EpsGraphAttributes& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif
