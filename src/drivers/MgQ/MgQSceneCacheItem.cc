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
    \file MgQSceneCacheItem.cc
    \brief Definition of MgQSceneCacheItem
    \author Graphics Section, ECMWF

    Started: September 2010
*/

#include "MgQSceneCacheItem.h"

#include <QDebug>
#include <QPainter>
#include <QPixmap>
#include <QStyleOptionGraphicsItem>

MgQSceneCacheItem::MgQSceneCacheItem(QPixmap* pixmap,QGraphicsItem* parent) : 
	QGraphicsItem(parent), pixmap_(pixmap)
{
	setFlag(QGraphicsItem::ItemUsesExtendedStyleOption,true);
}

MgQSceneCacheItem::~MgQSceneCacheItem()
{	

}

QRectF MgQSceneCacheItem::boundingRect() const
{
	return clipRect_;
}

void MgQSceneCacheItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
{
	QRectF rect=option->exposedRect;
	
	/*if(!painter->clipRegion().isEmpty());
	{	  		
		clipRect=painter->clipRegion().boundingRect();
	}
	else
	{
	  	clipRect=painter->clipPath().boundingRect();
	}*/
	
	painter->drawPixmap(rect,*pixmap_,rect);	
}

