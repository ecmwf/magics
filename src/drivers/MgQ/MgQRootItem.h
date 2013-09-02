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
    \file MgQRootItem.h
    \brief Definition of MgQRootItem.
    \author Graphics Section, ECMWF

    Started: June 2011
*/

#ifndef _MgQRootItem_H
#define _MgQRootItem_H

#include <magics.h>
#include "MgQ.h"

class MgQRootItem : public QGraphicsItem
{
public:
   	MgQRootItem(QGraphicsItem* parent=0) : QGraphicsItem(parent) {};
	virtual ~MgQRootItem() {};
	
	int type() const {return Type;}
	QRectF boundingRect() const {return QRectF();}
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *) {};
		
	void clearContents()
	{
		foreach(QGraphicsItem* item,childItems())
		{
	  		scene()->removeItem(item);
			delete item;
		}
	}
};



#endif
