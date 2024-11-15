
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileFortranViewNodeAttributes.h
    \brief Definition of FortranViewNode Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef FortranViewNodeAttributes_H
#define FortranViewNodeAttributes_H

#include "magics.h"
#include "Transformation.h"
#include "Colour.h"
#include "PreviewVisitor.h"
#include "MagnifierVisitor.h"
namespace magics {

class XmlNode;
class FortranViewNodeAttributes 
{
public:
//  --  constructor
    FortranViewNodeAttributes();
    
//  --  destructor
    virtual ~FortranViewNodeAttributes();
    
    virtual void set(const std::map<std::string, std::string>&);
    virtual void set(const XmlNode&);
    virtual void copy(const FortranViewNodeAttributes&);
    virtual bool accept(const std::string&);

    void setTag(const std::string& tag) { tag_ = tag; }

public:
	//  --  method
	virtual void print(std::ostream&) const;
	virtual void toxml(std::ostream& out) const;
	//  --  members:
	string tag_;
	double left_;
	double bottom_;
	double width_;
	double height_;
	bool predefined_;
	string predefined_name_;
	double bottom_internal_;
	double left_internal_;
	double right_;
	double height_internal_;
	double width_internal_;
	double top_;
	bool clipping_;
	bool frame_;
	int frame_thickness_;
	double vertical_axis_with_;
	double horizontal_axis_height_;
	string overlay_;
	string horizontal_;
	string vertical_;
	string json_;
	bool title_;
	bool expand_;
	unique_ptr<Transformation> transformation_;
	unique_ptr<Colour> background_;
	unique_ptr<Colour> frame_colour_;
	LineStyle frame_line_style_;
	unique_ptr<NoPreviewVisitor> preview_;
	unique_ptr<NoMagnifierVisitor> magnify_;
	 

private:
	friend ostream& operator<<(ostream& s,const FortranViewNodeAttributes& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif

