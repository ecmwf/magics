/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/*! \file ArrowProperties.cc
    \brief Implementation of the Template class Arrow.
    
    Magics Team - ECMWF 2005
    
    Started: Wed 16-Mar-2005
    
    Changes:
    
*/

#include "ArrowProperties.h"
#include "OriginMarker.h"
#include "CalmIndicator.h"

using namespace magics;

ArrowProperties::ArrowProperties() :
	scale_(0),  crossBoundary_(false), 
	thickness_(1), style_(M_SOLID), hemisphere_(NORTH), position_(M_TAIL), 
	originMarker_("none"), originHeight_(0),headIndex_(0),headRatio_(0.3)
{
}

void ArrowProperties::copy(const ArrowProperties& from) 
{
	scale_         = from.scale_;
	crossBoundary_ = from.crossBoundary_;
	thickness_     = from.thickness_;
	style_         = from.style_;
	hemisphere_    = from.hemisphere_;
	position_      = from.position_;
	originMarker_  = from.originMarker_;
	originHeight_  = from.originHeight_;
	headIndex_     = from.headIndex_;
	headRatio_     = from.headRatio_;
}


void ArrowProperties::print(ostream& out)  const
{
	out << ", scale=" << scale_;
	out << ", crossBoundary=" << crossBoundary_;
	out << ", thickness=" << thickness_;
	out << ", style=" << style_;
	out << ", hemisphere=" << hemisphere_;
	out << ", originMarker_=" << originMarker_;
	out << ", originHeight_=" << originHeight_;
}

