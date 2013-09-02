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
    \file MgQTextItem.cc
    \brief Definition of MgQTextItem
    \author Graphics Section, ECMWF

    Started: March2010
*/

#include "MgQTextItem.h"

#include <QDebug>
#include <QPainter>


MgQTextItem::MgQTextItem(const QString &text,QGraphicsItem* parent) : 
	QGraphicsSimpleTextItem(text,parent),  textBlanking_(false)
{

}

MgQTextItem::~MgQTextItem()
{	

}

QRectF MgQTextItem::boundingRect() const
{
	return QGraphicsSimpleTextItem::boundingRect();
}

void MgQTextItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                QWidget *widget)
{
	if(textBlanking_)
	{
		painter->fillRect(boundingRect(),Qt::white);			
	}

	QGraphicsSimpleTextItem::paint(painter,option,widget);

}
