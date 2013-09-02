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
    \file MgQPathItem.cc
    \brief Definition of MgQPathItem
    \author Graphics Section, ECMWF

    Started: March 2010
*/

#include "MgQPathItem.h"

#include <QDebug>
#include <QPainter>

//static int count=0;


MgQPathItem::MgQPathItem(const QPainterPath& path,QGraphicsItem* parent) : 
	QGraphicsPathItem(path,parent)
{

}

MgQPathItem::~MgQPathItem()
{	

}

QRectF MgQPathItem::boundingRect() const
{
	//qDebug() << "PIXMAP" << QGraphicsPathItem::boundingRect();
	//qDebug() << "PIXMAP" << targetRect_;

	//return QGraphicsPathItem::boundingRect();
	float w=boundingRectSize_;
	return QRectF(-w/2.,-w/2.,w/2.,w/2.);
}

void MgQPathItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
{
	/*if(parentItem()->data(3).toRectF() != QRectF())
	{		
		QPolygonF p= mapFromScene(parentItem()->data(3).toRectF()); 
		if(p.boundingRect().intersects(boundingRect()) == false)
		{
			return;
		}
	}*/

	QGraphicsPathItem::paint(painter,option,widget);

	//count++;

	//qDebug() << "count" << count;	

	//QGraphicsPathItem::paint(painter,option,widget);
	//painter->drawPath(QRectF(0,0,targetRect_.width(),targetRect_.height()),
	//		    path(),
	//		    QRectF(0,0,pixmap().width(),pixmap().height()));
}
