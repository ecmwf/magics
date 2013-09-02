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
    \file MgQPathItem.h
    \brief Definition of MgQPathItem.
    \author Graphics Section, ECMWF

    Started: March 2010
*/

#ifndef _MgQPathItem_H
#define _MgQPathItem_H

#include <QGraphicsPathItem>

#include "MgQ.h"

class MgQPathItem : public QGraphicsPathItem
{
public:
	enum {Type = MgQ::PathItemType}; 
	
	MgQPathItem(const QPainterPath& ,QGraphicsItem* parent = 0);
	~MgQPathItem();
	
	void setBoundingRectSize(float s) {boundingRectSize_=s;}
	QRectF boundingRect() const;
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *widget=0);
	int type() const {return Type;}

protected:
	float boundingRectSize_;
};	

#endif
