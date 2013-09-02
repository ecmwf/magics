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
    \file MgQPixmapItem.cc
    \brief Definition of MgQPixmapItem
    \author Graphics Section, ECMWF

    Started: February 2010
*/

#include "MgQPixmapItem.h"

#include <QDebug>
#include <QPainter>

MgQPixmapItem::MgQPixmapItem(const QPixmap& pixmap,QGraphicsItem* parent) : 
	QGraphicsPixmapItem(pixmap,parent)
{

}

MgQPixmapItem::~MgQPixmapItem()
{	

}

QRectF MgQPixmapItem::boundingRect() const
{
	//qDebug() << "PIXMAP" << QGraphicsPixmapItem::boundingRect();
	//qDebug() << "PIXMAP" << targetRect_;

	//return QGraphicsPixmapItem::boundingRect();
	return QRectF(0,0,targetRect_.width(),targetRect_.height());
}

void MgQPixmapItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
{
	if(!clipRect_.isEmpty())
		painter->setClipRect(clipRect_);

	painter->setBackgroundMode(Qt::TransparentMode);

	//QGraphicsPixmapItem::paint(painter,option,widget);
	painter->drawPixmap(QRectF(0,0,targetRect_.width(),targetRect_.height()),
			    pixmap(),
			    QRectF(0,0,pixmap().width(),pixmap().height()));
}

void MgQPixmapItem::setClipRect(QRectF rect)
{
	clipRect_=rect;
}
