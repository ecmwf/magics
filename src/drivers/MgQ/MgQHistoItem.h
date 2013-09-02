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
    \file MgQHistoItem.h
    \brief Definition of MgQHistoItem.
    \author Graphics Section, ECMWF

    Started: February 2011
*/

#ifndef _MgQHistoItem_H
#define _MgQHistoItem_H

#include <magics.h>

#include "MgQ.h"

class MgQHistoItem : public QGraphicsItem 
{
public:
	enum {Type = MgQ::HistoItemType}; 

	MgQHistoItem();
	~MgQHistoItem();
	QPixmap pixmap() {return pixmap_;}
	void setPixmap(QPixmap p,QSize s) {pixmap_=p;requestedPixmapSize_=s;} 
	QSize requestedPixmapSize() {return requestedPixmapSize_;} 
	bool cached() {return cached_;};
	void setCached(bool b) {cached_=b;}	
	QHash<QString,QString> pixmapId() {return pixmapId_;}
	void setPixmapId(QHash<QString,QString> id) {pixmapId_=id;}
	
	int type() const {return Type;}
	QRectF boundingRect() const {return QRectF();}
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *) {};

protected:
	QPixmap pixmap_;
	QSize requestedPixmapSize_;
	bool cached_;
	QHash<QString,QString> pixmapId_;
};

#endif
