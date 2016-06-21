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
