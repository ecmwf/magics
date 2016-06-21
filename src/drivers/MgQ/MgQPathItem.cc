/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

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
