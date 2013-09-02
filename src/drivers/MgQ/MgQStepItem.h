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
    \file MgQStepItem.h
    \brief Definition of MgQStepItem.
    \author Graphics Section, ECMWF

    Started: May 2008
*/

#ifndef _MgQStepItem_H
#define _MgQStepItem_H

#include <magics.h>

#include "MgQ.h"

class MgQLayoutItem;

class MgQStepItem : public QGraphicsItem
{
public:
	enum {Type = MgQ::StepItemType}; 
		
	MgQStepItem(MgQLayoutItem*);
	~MgQStepItem();
	
	int     id() {return id_;}
	void    id(int i) {id_=i;}
	bool    cached() {return cached_;}
	void    setCached(bool b) {cached_=b;}
	MgQLayoutItem* parentLayoutItem() {return parentLayoutItem_;}
	
	int type() const {return Type;}
	QRectF boundingRect() const {return QRectF();}
	void paint(QPainter *, const QStyleOptionGraphicsItem *,
                QWidget *) {};

protected:
	int         	id_;
	bool        	cached_;			
	MgQLayoutItem   *parentLayoutItem_;
};


//}


#endif
