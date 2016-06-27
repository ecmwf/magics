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
    \file MgQSceneCacheItem.h
    \brief Definition of MgQSceneCacheItem.
    \author Graphics Section, ECMWF

    Started: September 2011
*/

#ifndef _MgQSceneCacheItem_H
#define _MgQSceneCacheItem_H

#include <QGraphicsItem>

#include "MgQ.h"

class MgQSceneCacheItem : public QGraphicsItem
{
public:
	enum {Type = MgQ::SceneCacheItemType}; 
	
	MgQSceneCacheItem(QPixmap*,QGraphicsItem* parent = 0);
	~MgQSceneCacheItem();
	
	QRectF boundingRect() const;
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *widget=0);
	void setPixmap(QPixmap* p) {pixmap_=p;}
	void setClipRect(QRectF r) {clipRect_=r;}
	int type() const {return Type;}

protected:
	QRectF clipRect_;
	QPixmap* pixmap_;

};	


#endif 
