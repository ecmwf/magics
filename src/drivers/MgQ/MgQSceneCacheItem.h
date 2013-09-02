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
