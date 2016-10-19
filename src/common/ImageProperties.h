/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ImageProperties.h
    \brief Definition of the Template class ImageProperties.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 13-Apr-2005
    
    Changes:
    
*/

#ifndef ImageProperties_H
#define ImageProperties_H

#include "magics.h"
#include "BasicGraphicsObject.h"
#include "ColourTable.h"
#include "Transformation.h"


namespace magics {

class ImageProperties : public BasicGraphicsObject {

public:
	ImageProperties() : transparency_(1), originReference_(centre) {}
	virtual ~ImageProperties() {}

	enum  OriginReference { centre, bottom_left };

    void copy(const ImageProperties& from) {
    	origin_ = from.origin_;
    	width_  = from.width_;
    	height_ = from.height_;
    	transparency_ = from.transparency_;
    	table_ = from.table_;
    }
	
	void setOrigin(const PaperPoint& origin)    { origin_ = origin; }
	PaperPoint getOrigin() const                { return origin_; }
	
	void setWidth(double width)      { width_ = width; }
	double getWidth() const          { return width_; }
	
	void setHeight(double height)    { height_ = height; }
	double getHeight() const         { return height_; }
	
	void setTransparency(double transparency)    { transparency_ = transparency; }
	double getTransparency() const         { return transparency_; }
	
	void setColourTable(const ColourTable& table) { table_ = table; }
	ColourTable& getColourTable() const { return table_; }
	
	OriginReference getOriginReference() const { return originReference_; }
	void setOriginReference(OriginReference ref)  { originReference_ = ref; }
protected:
     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream& out) const {
	 	out << "ImageProperties[x=" << origin_.x(); 
		out << ", y=" << origin_.y();
		out << ", width=" << width_;
		out << ", height=" << height_;
		out << ", lookup_table=" << table_;
		out << ", transparency=" << transparency_;
		out << "]";
	 }
	 PaperPoint origin_;
	 double width_;
	 double height_;
	 mutable ColourTable table_;
	 double transparency_;

	 OriginReference originReference_;


private:
    //! Copy constructor - No copy allowed
	ImageProperties(const ImageProperties&);
    //! Overloaded << operator to copy - No copy allowed
	ImageProperties& operator=(const ImageProperties&);
    
// -- Friends
    //! Overloaded << operator to call print().
	friend ostream& operator<<(ostream& s,const ImageProperties& p)
		{ p.print(s); return s; }

};

} // namespace magics


#endif
