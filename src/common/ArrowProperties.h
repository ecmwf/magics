/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ArrowProperties.h
    \brief Definition of the Template class Arrow.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 16-Mar-2005
    
    Changes:
    
*/

#ifndef ArrowProperties_H
#define ArrowProperties_H

#include "magics.h"
#include "Colour.h"

namespace magics {

enum FlagConvention  { SI , KNOTS  };
class ArrowProperties 
{
public:
	ArrowProperties();
	virtual ~ArrowProperties(){}
	ArrowProperties* clone() {
		ArrowProperties* arrow = new ArrowProperties();
		arrow->copy(*this);
		return arrow;
	}

// Accessors :
	double getScale() const                { return scale_; }
	void setScale(double scale)            { scale_ = scale; }

	bool getCrossBoundary() const            { return crossBoundary_; }
	void setCrossBoundary(bool cross)        { crossBoundary_ = cross; }

	int getThickness() const              { return thickness_; }
	void setThickness(int thickness)        { thickness_ = thickness; }

	LineStyle getStyle() const               { return style_; }
	void setStyle(LineStyle style)           { style_ = style; }

	ArrowPosition getArrowPosition() const   { return position_; }
	void setArrowPosition(ArrowPosition pos) { position_ = pos; }

	void copy(const ArrowProperties&);

	Hemisphere getHemisphere() const          { return hemisphere_; }
	void setHemisphere(Hemisphere hemisphere) { hemisphere_ = hemisphere; }

	string getOriginMarker() const          { return originMarker_; }
	void setOriginMarker(const string& marker) { originMarker_ = marker; }

	double getOriginHeight() const          { return originHeight_; }
	void setOriginHeight(double height)     { originHeight_ = height; }

	void setHeadIndex(int head)		{ headIndex_ =  head; } 
	int getHeadIndex() const		{ return headIndex_; } 

	void setHeadRatio(double ratio)		{ headRatio_ =  ratio; } 
	double getHeadRatio() const		{ return headRatio_; } 
    
	void setColour(const Colour& colour)		{ colour_ =  colour; } 
	const Colour& getColour() const		{ return colour_; } 
protected:

     //! Method to print string about this class on to a stream of type ostream (virtual).
	 virtual void print(ostream&) const;

	 double      scale_;
	 bool          crossBoundary_;
	 int           thickness_;
	 LineStyle     style_;
	 Colour colour_;
	 Hemisphere    hemisphere_;
	 ArrowPosition position_;
	 string        originMarker_;
	 double      originHeight_;
	 int           headIndex_;
	 double        headRatio_;
};
}// end namespace

#endif
