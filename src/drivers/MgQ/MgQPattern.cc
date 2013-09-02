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

/*!
    \file MgQPattern.h
    \brief Definition of MgQPattern
    \author Graphics Section, ECMWF

    Started: Feb 2011
*/

#include "MgQPattern.h"

#include <QDebug>
#include <QPainter>

//====================================
//
//  Properties
//
//====================================

MgQPatternProperties::MgQPatternProperties(Type type) : type_(type), bg_(QColor(255,255,255,0))
{

}

bool MgQPatternProperties::operator==(const MgQPatternProperties& p) const
{
	if(p.type_ != type_) 
		return false;

	if(type_==DotShading)
	{
		return p.size_ == size_ && p.itemSize_ == itemSize_ &&
		       p.bg_ == bg_ && p.colour_ == colour_;

	}
	else if(type_==HatchShading)
	{
		return p.size_ == size_ && p.id_ == id_ && p.lineWidth_ == lineWidth_ &&
		       p.bg_ == bg_ && p.colour_ == colour_;

	}
}

//====================================
//
//  Pattern
//
//====================================

MgQPattern::MgQPattern(const MgQPatternProperties &prop) : QPixmap(prop.size_), prop_(prop)
{
	if(prop.type_ == MgQPatternProperties::DotShading)
	{
		fill(prop.bg_);

		if(prop_.itemSize_.width() < 1)
			return;

		QPainter p(this);	

		if(prop_.itemSize_.width()== 1)
		{
			p.setPen(prop_.colour_);
			p.drawPoint(width()/2,height()/2);
			
		}
		else if(prop_.itemSize_.width() < 4)
		{
			p.fillRect(QRectF((width()-prop_.itemSize_.width())/2.,(height()-prop_.itemSize_.height())/2.,
			   prop_.itemSize_.width(),prop_.itemSize_.height()),
			   prop_.colour_);
		}
		else
		{
			p.setRenderHint(QPainter::Antialiasing,true);
			p.setPen(prop_.colour_);
			p.setBrush(prop_.colour_);
			p.drawEllipse(QRectF((width()-prop_.itemSize_.width())/2.,(height()-prop_.itemSize_.height())/2.,
			   prop_.itemSize_.width(),prop_.itemSize_.height()));
		}

	}
	else if(prop.type_ ==  MgQPatternProperties::HatchShading)
	{
		fill(prop.bg_);
			
		QPainter p(this);

		QPen pen(prop_.colour_);
		pen.setWidthF(prop_.lineWidth_);
		p.setPen(pen);

		int index = prop_.id_.toInt();

		int w=width();
		int h=height();

		if(index==1 || index==3) // horizontal
		{			
			p.drawLine(QPointF(0,h*0.5),QPointF(w-1,h*0.5));					
		}		
		if(index==2 || index==3) // vertical
		{			
			p.drawLine(QPointF(w*0.5,0),QPointF(w*0.5,h-1));					
		}
		if(index==4 || index==6) 
		{		
			p.drawLine(QPointF(0,0),QPointF(w-1,h-1));
		}
		if(index==5 || index==6) 
		{		
			p.drawLine(QPointF(0,h-1),QPointF(w-1,0));
		}
	}
}

//====================================
//
//  Manager
//
//====================================

MgQPatternManager::~MgQPatternManager()
{	
	foreach(MgQPattern *item, patterns_)
	{		
		delete item;
	}
}

MgQPattern* MgQPatternManager::getPattern(MgQPatternProperties& p)
{	
	foreach(MgQPattern *item, patterns_) 
	{
		if( item->properties() == p)
		{
			return item;
		}
	}
	
	return 0;		
}

MgQPattern* MgQPatternManager::addPattern(MgQPatternProperties& p)
{
	MgQPattern *pix = getPattern(p);

	if(pix) 
	{
		return pix;
	}
	
	pix=new MgQPattern(p);
	patterns_ <<  pix;
	return pix;	
}

void MgQPatternManager::deletePattern(MgQPatternProperties& p)	
{	
	foreach(MgQPattern *item, patterns_)
	{
		if( item->properties() == p)
		{
			delete item;
			return;
		}	
	}
}	

