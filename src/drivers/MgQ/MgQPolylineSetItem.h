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
    \file MgQPolylineSetItem.h
    \brief Definition of MgQPolylineSetItem.
    \author Graphics Section, ECMWF

    Started: April 2010
*/

#ifndef _MgQPolylineSetItem_H
#define _MgQPolylineSetItem_H

#include <QGraphicsItem>

#include "MgQ.h"

class MgQPolyline
{
public:
	MgQPolyline() : points_(0), path_(0), num_(0) {};
	QPointF* points_;
	QPainterPath* path_; //polygons with holes stored as QPainterPath
	int num_;
	int brushIndex_;
	int penIndex_;
	bool isPolygon_;
}; 

class MgQPolylineSetItem : public QGraphicsItem
{
public:
	enum {Type = MgQ::PolylineSetItemType}; 
	
	MgQPolylineSetItem(QRectF &,QGraphicsItem* parent = 0);
	~MgQPolylineSetItem();
	
	QRectF boundingRect() const;
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *widget=0);
	int type() const {return Type;}

	void addPolyline(QVector<QPointF>,QBrush,QPen,bool);
	void addPath(QPainterPath&,QBrush,QPen);

protected:
	QList<MgQPolyline*> polylines_;
	QList<QBrush> brushList_;
	QList<QPen> penList_;

	QRectF boundingRect_;
};

#endif 
