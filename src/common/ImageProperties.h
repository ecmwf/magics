/******************************** LICENSE ********************************

 Copyright 2007 European Centre for Medium-Range Weather Forecasts (ECMWF)

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

    http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

 ******************************** LICENSE ********************************/

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
