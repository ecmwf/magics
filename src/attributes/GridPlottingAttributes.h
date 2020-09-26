
/*******************************  LICENSE  *******************************

 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.

 *******************************  LICENSE  *******************************/

/*! \fileGridPlottingAttributes.h
    \brief Definition of GridPlotting Attributes class.

    This file is automatically generated.
    Do Not Edit!

    Generated: 
*/
   

#ifndef GridPlottingAttributes_H
#define GridPlottingAttributes_H

#include "magics.h"
#include "Colour.h"
namespace magics {

class XmlNode;
class GridPlottingAttributes 
{
public:
//  --  constructor
    GridPlottingAttributes();
    
//  --  destructor
    virtual ~GridPlottingAttributes();
    
    virtual void set(const std::map<std::string, std::string>&);
    virtual void set(const XmlNode&);
    virtual void copy(const GridPlottingAttributes&);
    virtual bool accept(const std::string&);

    void setTag(const std::string& tag) { tag_ = tag; }

public:
	//  --  method
	virtual void print(std::ostream&) const;
	virtual void toxml(std::ostream& out) const;
	//  --  members:
	string tag_;
	int thickness_;
	bool grid_frame_;
	int grid_frame_thickness_;
	LineStyle style_;
	unique_ptr<Colour> colour_;
	LineStyle grid_frame_style_;
	unique_ptr<Colour> grid_frame_colour_;
	 

private:
	friend ostream& operator<<(ostream& s,const GridPlottingAttributes& p)
	{ p.print(s); return s; }
};

} // namespace magics

#endif
